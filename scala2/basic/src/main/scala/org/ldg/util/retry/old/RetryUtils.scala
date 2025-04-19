package org.ldg.util.retry.old

import java.io.FileNotFoundException
import java.util.concurrent.{CompletableFuture, Executor, TimeUnit}
import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.FutureConverters._
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

object RetryUtils {

  // note: this method should be wrapped with handlers that consider the full set of exceptions from all libraries
  // Some rules of thumb for when to not retry an exceptions:
  //   - resource exhaustion (e.g. disk space, memory, etc)
  //   - bad request/illegal argument (e.g. http code most 400s but not 408/429)
  //   - mutations that violate constraints (e.g. duplicate key, object exists/does not exist, invalid data, etc)
  //   - authn/authz errors (e.g. not logged in, not authorized, http code 401/403)
  //   - redirection (http code 300s)
  //   - some http 500s (501, 505,507?,511)
  //   - bad responses (e.g. failure to parse payload)
  val defaultShouldRetry: Throwable => Boolean = {
    // IOException
    case _:FileNotFoundException => false

    case _:ReflectiveOperationException => false

    // RuntimeException
    case _:IllegalArgumentException => false
    case _:ClassCastException => false
    case _:TypeNotPresentException => false
    case _:UnsupportedOperationException => false

    case _:CloneNotSupportedException => false
    case ex => NonFatal(ex)
  }

  // 32bit int = 30
  private val RETRY_DELAY_ATTEMPT_COUNT_MAX: Int = Math.floor(Math.log(Integer.MAX_VALUE) / Math.log(2)).toInt

  def retryDelayWithExponentialBackoffAndJitter(
    initialDelay: FiniteDuration,
    maxJitter: FiniteDuration,
    maxDelay: FiniteDuration
  )(
    attemptCount: Int
  ): FiniteDuration = {
    require(attemptCount > 0)
    val attemptZeroCount = attemptCount - 1
    val limitedAttempts = Math.min(attemptZeroCount, RETRY_DELAY_ATTEMPT_COUNT_MAX)
    val initialDelayNs = initialDelay.toNanos

    /* Multiplier
      [1st attempt] 0=1.0
      1=2
      2=4
      3=8
      4=16
      5=32
      6=64
      7=128
      8=256
      9=512
      10=1024
    */
    val expDelayMultiplier = 1L << limitedAttempts
    val expDelayNs = initialDelayNs * expDelayMultiplier
    val limitedExpDelayNs = Math.min(expDelayNs, maxDelay.toNanos)
    val jitterNs = Random.nextLong(maxJitter.toNanos + 1)
    val totalNs = limitedExpDelayNs + jitterNs
    Math.min(totalNs, maxDelay.toNanos).nanos
  }

  val defaultRetryDelayWithExponentialBackoffAndJitter: Int => FiniteDuration =
    retryDelayWithExponentialBackoffAndJitter(
      initialDelay = 250.millis,
      maxJitter = 10.millis,
      // note: with initialDelay 250ms, retry 6+ (16s) limited to 10 seconds
      maxDelay = 10.seconds
    )

  def recurseUntilUltimateCause(cause: Throwable): Throwable =
    Option(cause.getCause).fold(cause)(recurseUntilUltimateCause)

  def runRetryState[A, S](
    onBeforeAttempt: S => Future[S],
    onAfterFailure:  (S, Throwable) => Future[Option[(S, FiniteDuration)]]
  )(
    initialState: S
  )(
    f: S => Future[A]
  )(implicit
    executionContext: ExecutionContext
  ): Future[(S, Try[A])] = {
    def attempt(state0: S, executor: Executor ): Future[(S, Try[A])] =
      for {
        state1 <- onBeforeAttempt(state0)
        tuple <- f( state1 )
          .transformWith {
            case Success(a) =>
              Future.successful((state1, Success(a)))
            case Failure(cause) if NonFatal(recurseUntilUltimateCause(cause)) =>
              for {
                maybeState2 <- onAfterFailure(state1, cause)
                tuple <- maybeState2 match {
                  case Some((state2, retryDelay)) =>
                    CompletableFuture.supplyAsync(
                      () => attempt( state2, executor ),
                      CompletableFuture.delayedExecutor(
                        retryDelay.toNanos,
                        TimeUnit.NANOSECONDS,
                        executor
                      )
                    ).asScala.flatten
                  case None =>
                    Future.successful((state1, Failure(cause)))
                }
              } yield tuple
            // don't retry fatal exceptions
            case failure@Failure(cause) =>
              Future.successful((state1, failure))
          }
      } yield tuple

    attempt( initialState, runnable => executionContext.execute(runnable) )
  }

}
