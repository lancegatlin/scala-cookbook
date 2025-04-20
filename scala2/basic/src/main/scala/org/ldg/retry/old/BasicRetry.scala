package org.ldg.retry.old

import ch.qos.logback.classic.Level
import com.typesafe.scalalogging.{LazyLogging, Logger}

import java.util.UUID
import scala.concurrent.duration.{Duration, DurationLong, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object BasicRetry extends LazyLogging {
  import RetryUtils._

  /**
   * Retry a function until it succeeds or exceeds max attempts or exceeds max elapsed time or throws an unretryable 
   * exception
   *
   * Example of retrying a code block at most 5 times using default settings:
   *   val doEffect: String => Future[String]
   *   retry(5)(doEffect("example"))
   *   
   * Example of retrying a code block for at most 1 minute using default settings:
   *   retry(1.minute)(doEffect("example"))
   *
   * Example of retrying with configuration:
   *   retry(5)
   *     .shouldRetry {
   *       case ex:RuntimeException => false
   *     }
   *     .onRetryEvent {
   *       case RetryEvent.OnAfterFailure(_, cause, RetryAction.AttemptAgain(_)) =>
   *         println(s"retrying: $cause")
   *       case _ => // do nothing
   *     }
   *     .calcRetryDelay((_,_) => 5.seconds)
   *     .run(doEffect("example"))
   *
   * Example of wrapping an existing function with a saved RetryPlan:
   *   import BasicRetry.Implicits._
   *   val retryPlan =
   *     retry(5)
   *       .shouldRetry {
   *         case _:RuntimeException => false
   *       }
   *   val doEffectWithRetry: String => Future[String] = doEffect.withRetry(retryPlan)
   *   val doEffectWithRetry2: String => Future[String] = doEffect.withRetry(retry(5))
   *
   * @param maxAttempts maximum number of attempts
   * @param maxElapsedTime maximum elapsed time since start of retry call to run another attempt (set to Duration.Inf
   *                       for no time limit) Note: this is not a timeout and will not interrupt a retry in progress
   * @return a RetryPlan
   */
  def retry(
     maxAttempts: Int,
     maxElapsedTime: Duration
  ): RetryPlan = RetryPlan(maxAttempts, maxElapsedTime)

  def retry(maxAttempts: Int): RetryPlan = RetryPlan(maxAttempts, Duration.Inf)

  def retry(maxElapsedTime: Duration): RetryPlan = RetryPlan(Int.MaxValue, maxElapsedTime)

  
  case class RetryPlan(
    maxAttempts: Int,
    maxElapsedTime: Duration,
    maybePartialShouldRetry: Option[PartialFunction[Throwable, Boolean]] = None,
    maybeRetryDelay: Option[(RetryState, Throwable) => FiniteDuration] = None,
    maybeOnRetryEvent: Option[RetryEvent => Unit] = None,
    maybeCorrelationId: Option[String] = None
  ) {
    def shouldRetry(f: PartialFunction[Throwable, Boolean]): RetryPlan =
      copy(maybePartialShouldRetry = Some(f))

    def onRetryEvent(f: RetryEvent => Unit): RetryPlan =
      copy(maybeOnRetryEvent = Some(f))

    def retryDelay(f: (RetryState, Throwable) => FiniteDuration): RetryPlan =
      copy(maybeRetryDelay = Some(f))

    def correlationId(id: String): RetryPlan =
      copy(maybeCorrelationId = Some(id))

    def apply[A](
      f: => Future[A]
    )(implicit
      retryConfig: RetryConfig,
      executionContext: ExecutionContext
    ): Future[A] = run(f)

    def toRetryConfig(implicit baseRetryConfig: RetryConfig): RetryConfig = RetryConfig(
      shouldRetry = maybePartialShouldRetry match {
        case None => baseRetryConfig.shouldRetry
        case Some(partialShouldRetry) =>
        { ex:Throwable => partialShouldRetry.applyOrElse(ex, baseRetryConfig.shouldRetry) }
      },
      retryDelay = maybeRetryDelay.getOrElse(baseRetryConfig.retryDelay),
      onRetryEvent = maybeOnRetryEvent.getOrElse(baseRetryConfig.onRetryEvent),
      genCorrelationId = { () => maybeCorrelationId.getOrElse(baseRetryConfig.genCorrelationId()) }
    )

    def run[A](
      f: RetryState => Future[A]
    )(implicit
      baseRetryConfig: RetryConfig,
      executionContext: ExecutionContext
    ): Future[A] = {
      val retryConfig: RetryConfig = toRetryConfig(baseRetryConfig)

      runRetryState[A, RetryState](
        onBeforeAttempt = { retryState =>
          Future.successful(retryState.onBeforeAttempt())
        },
        onAfterFailure = { (retryState, cause) =>
          Future.successful(retryState.onAfterFailure(cause))
        }
      )(
        initialState = RetryState.initial(
          maxAttempts = maxAttempts,
          maxElapsedTime = maxElapsedTime,
          correlationId = retryConfig.genCorrelationId()
        )(
          retryConfig = retryConfig
        )
      )(f)
        .flatMap {
          case (_, Failure(ex)) =>
            Future.failed(ex)
          case (retryState, Success(a)) =>
            retryState.onRetrySuccess()
            Future.successful(a)
        }
    }

    def run[A](
      f: => Future[A]
    )(implicit
      implicitRetryConfig: RetryConfig,
      executionContext: ExecutionContext
    ): Future[A] = run(_ => f)
  }

  case class RetryConfig(
    shouldRetry: Throwable => Boolean,
    retryDelay: (RetryState, Throwable) => FiniteDuration,
    onRetryEvent: RetryEvent => Unit,
    genCorrelationId: () => String
  )

  object RetryConfig {
    implicit val default: RetryConfig = RetryConfig(
      shouldRetry = defaultShouldRetry,
      retryDelay = { (retryState, _) =>
        defaultRetryDelayWithExponentialBackoffAndJitter(retryState.attemptCount)
      },
      onRetryEvent = defaultLoggerRetryEventHandler,
      genCorrelationId = defaultGenCorrelationId
    )
  }

  case class RetryState(
    correlationId: String,
    attemptCount: Int,
    maxAttempts: Int,
    startTimeNs: Long,
    maxElapsedTime: Duration,
    lastModifiedNs: Long
)(implicit
   retryConfig: RetryConfig
) {
    def elapsed: FiniteDuration = (lastModifiedNs - startTimeNs).nanos

    def onBeforeAttempt(): RetryState = {
      val newState = copy(
        attemptCount = attemptCount + 1,
        lastModifiedNs = System.nanoTime()
      )
      newState
    }

    def onAfterFailure(cause: Throwable): Option[(RetryState, FiniteDuration)] = {
      import RetryEvent._
      val ultimateCause = recurseUntilUltimateCause(cause)
      val newState = copy(
        lastModifiedNs = System.nanoTime()
      )

      if (newState.elapsed <= maxElapsedTime) {
        if (retryConfig.shouldRetry(ultimateCause)) {
          if (attemptCount < maxAttempts) {
            val retryDelay = retryConfig.retryDelay(newState, cause)
            retryConfig.onRetryEvent(OnAfterFailure(newState, cause, AttemptAgain(retryDelay)))
            Some(newState, retryDelay)
          } else {
            retryConfig.onRetryEvent(RetryEvent.OnAfterFailure(newState, cause, RethrowFailure(ExhaustedMaxAttempts)))
            None
          }
        } else {
          retryConfig.onRetryEvent(RetryEvent.OnAfterFailure(newState, cause, RethrowFailure(UnretryableFailure)))
          None
        }
      } else {
        retryConfig.onRetryEvent(RetryEvent.OnAfterFailure(newState, cause, RethrowFailure(ExceededMaxElapsedTime)))
        None
      }
    }

    def onRetrySuccess(): RetryState = {
      if(attemptCount > 1) {
        retryConfig.onRetryEvent(RetryEvent.OnRetrySuccess(this))
      }

      copy(
        lastModifiedNs = System.nanoTime()
      )
    }
  }

  object RetryState {
    def initial(
      maxAttempts: Int,
      maxElapsedTime: Duration,
      correlationId: String
    )(implicit
      retryConfig: RetryConfig
    ): RetryState = {
      val now = System.nanoTime()
      RetryState(
        correlationId = correlationId,
        attemptCount = 0,
        maxAttempts = maxAttempts,
        startTimeNs = now,
        maxElapsedTime = maxElapsedTime,
        lastModifiedNs = now
      )
    }

    implicit def fmtRetryState(retryState: RetryState): String = {
      import retryState._
      val sMaxAttempts = if(maxAttempts == Int.MaxValue) "-" else maxAttempts.toString
      val sMaxElapsedTime = if(maxElapsedTime == Duration.Inf) "-" else maxElapsedTime.toMillis.millis
      s"attempts=$attemptCount/$sMaxAttempts elapsed=${elapsed.toMillis.millis}/$sMaxElapsedTime correlationId=$correlationId"
    }
  }

  sealed trait RetryEvent
  object RetryEvent {
    case class OnAfterFailure(retryState: RetryState, cause: Throwable, action: RetryAction) extends RetryEvent
    // only occurs if there was at least one retry
    case class OnRetrySuccess(retryState: RetryState) extends RetryEvent


    sealed trait RethrowReason
    case object UnretryableFailure extends RethrowReason
    case object ExhaustedMaxAttempts extends RethrowReason
    case object ExceededMaxElapsedTime extends RethrowReason


    sealed trait RetryAction
    case class AttemptAgain(retryDelay: FiniteDuration) extends RetryAction
    case class RethrowFailure(reason: RethrowReason) extends RetryAction
  }


  def stderrRetryEventHandler(logger: Logger)(retryEvent: RetryEvent): Unit =
    loggingRetryEventHandler(
      log = (level, msg) => System.err.println(s"[$level] $msg")
    )(retryEvent)

  def loggerRetryEventHandler(logger: Logger)(retryEvent: RetryEvent): Unit =
    loggingRetryEventHandler(
      log = (level, msg) => level match {
        case Level.WARN => logger.warn(msg)
        case Level.ERROR => logger.error(msg)
        case Level.INFO => logger.info(msg)
        case Level.DEBUG => logger.debug(msg)
        case Level.TRACE => logger.trace(msg)
        case _ => throw new UnsupportedOperationException(s"unsupported log level: $level")
      }
    )(retryEvent)

  val defaultLoggerRetryEventHandler: RetryEvent => Unit = loggerRetryEventHandler(logger)

  def loggingRetryEventHandler(
    log: (Level, String) => Unit
  )(
    retryEvent: RetryEvent
  )(implicit
    fmtRetryState: RetryState => String
  ): Unit = {
    import RetryEvent._
    retryEvent match {
      case OnAfterFailure(retryState, cause, AttemptAgain(retryDelay)) =>
        log(Level.WARN, s"Retrying failure (${fmtRetryState(retryState)}) after $retryDelay delay: $cause")
      case OnAfterFailure(retryState, cause, RethrowFailure(reason))  =>
        log(Level.ERROR, s"Retry $reason (${fmtRetryState(retryState)}) rethrowing: $cause")
      case OnRetrySuccess(retryState) =>
        log(Level.INFO, s"Retry success (${fmtRetryState(retryState)})")
    }
  }

  def defaultGenCorrelationId(): String = UUID.randomUUID().toString

  object Implicits {
    implicit class BasicRetryFunctionExt[A,R](val self: A => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): A => Future[R] =
      { a => retryPlan.run(_ => self(a)) }
    }

    implicit class BasicRetryFunction2Ext[A,B,R](val self: (A,B) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B) => Future[R] =
      { (a,b) => retryPlan.run(_ => self(a,b)) }
    }

    implicit class BasicRetryFunction3Ext[A,B,C,R](val self: (A,B,C) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C) => Future[R] =
      { (a,b,c) => retryPlan.run(_ => self(a,b,c)) }
    }

    implicit class BasicRetryFunction4Ext[A,B,C,D,R](val self: (A,B,C,D) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C,D) => Future[R] =
      { (a,b,c,d) => retryPlan.run(_ => self(a,b,c,d)) }
    }

    implicit class BasicRetryFunction5Ext[A,B,C,D,E,R](val self: (A,B,C,D,E) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C,D,E) => Future[R] =
      { (a,b,c,d,e) => retryPlan.run(_ => self(a,b,c,d,e)) }
    }

    implicit class BasicRetryFunction6Ext[A,B,C,D,E,F,R](val self: (A,B,C,D,E,F) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C,D,E,F) => Future[R] =
      { (a,b,c,d,e,f) => retryPlan.run(_ => self(a,b,c,d,e,f)) }
    }

    implicit class BasicRetryFunction7Ext[A,B,C,D,E,F,G,R](val self: (A,B,C,D,E,F,G) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C,D,E,F,G) => Future[R] =
      { (a,b,c,d,e,f,g) => retryPlan.run(_ => self(a,b,c,d,e,f,g)) }
    }

    implicit class BasicRetryFunction8Ext[A,B,C,D,E,F,G,H,R](val self: (A,B,C,D,E,F,G,H) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C,D,E,F,G,H) => Future[R] =
      { (a,b,c,d,e,f,g,h) => retryPlan.run(_ => self(a,b,c,d,e,f,g,h)) }
    }

    implicit class BasicRetryFunction9Ext[A,B,C,D,E,F,G,H,I,R](val self: (A,B,C,D,E,F,G,H,I) => Future[R]) extends AnyVal {
      def withRetry(retryPlan: RetryPlan)(implicit retryConfig: RetryConfig, executionContext: ExecutionContext): (A,B,C,D,E,F,G,H,I) => Future[R] =
      { (a,b,c,d,e,f,g,h,i) => retryPlan.run(_ => self(a,b,c,d,e,f,g,h,i)) }
    }
  }
}
