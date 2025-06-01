package org.ldg.retry

import cats.effect.kernel.Clock
import cats.implicits.{catsSyntaxApplicativeError, catsSyntaxApplicativeId, catsSyntaxMonadErrorRethrow, toFlatMapOps, toFunctorOps}
import cats.{Applicative, Monad, MonadError}
import org.ldg.effect.Sleep

import scala.concurrent.duration.FiniteDuration

/**
  * A retry plan that defines the retry strategy for retrying an operation.
  *
  * @param maxAttempts the maximum number of attempts to retry (Int.MaxValue for unlimited)
  * @param maybeMaxElapsedDuration maybe a maximum elapsed time for retries
  * @param maybePfShouldRetry a partial function that defines which exceptions should be retried
  *                           Note: if set, this is called first. If unhandled then the baseRetryConfig shouldRetry
  *                           function is called.
  * @param maybePfCalcRetryDelay a function that calculates the delay between retries
  *                          Note: if set, this is called first. If unhandled then the baseRetryConfig calcRetryDelay
  *                          function is called.
  * @param maybeOnRetryEvent a function that is called on each retry event
  * @param maybeCorrelationId an optional correlation ID for the retry plan
  * @param baseRetryConfig base retry config
  * @tparam F the monadic context (e.g. IO, Future, Id)
  */
case class RetryPlan[F[_]](
    maxAttempts: Int,
    maybeMaxElapsedDuration: Option[FiniteDuration] = None,
    maybePfShouldRetry: Option[PartialFunction[( RetryState, Throwable ), F[Boolean]]] = None,
    maybePfCalcRetryDelay: Option[PartialFunction[( RetryState, Throwable ), F[FiniteDuration]]] = None,
    maybeOnRetryEvent: Option[RetryEvent => F[Unit]] = None,
    maybeCorrelationId: Option[String] = None
)(
    implicit
    val baseRetryConfig: RetryConfig[F] ) {

  /**
    * Copy a RetryPlan and set a new partial function for deciding if an exception should be retried. When set this
    * partial function is called first. If unhandled then the baseRetryConfig shouldRetry function is called.
    * Note1: to replace the base shouldRetry handler, modify the implicit RetryConfig in scope, e.g.:
    *   {{{
    *     implicit val myRetryConfig: RetryConfig[IO] = RetryConfig.default[IO]().copy(
    *       shouldRetry = ...
    *     )
    *   }}}
    * Note2: exceptions thrown in this partial function will cause the retry loop to fail immediately
    *
    * @param f a partial function that defines which exceptions should be retried
    * @return a new RetryPlan
    */
  def shouldRetry( f: PartialFunction[( RetryState, Throwable ), F[Boolean]] ): RetryPlan[F] =
    copy( maybePfShouldRetry = Some( f ) )

  /**
    * Copy a RetryPlan and set a new partial function for calculating the delay between retries. When set this partial
    * function is called first. If unhandled then the baseRetryConfig calcRetryDelay function is called.
    * Note1: to replace the base calcRetryDelay handler, modify the implicit RetryConfig in scope, e.g.:
    *   {{{
    *     implicit val myRetryConfig: RetryConfig[IO] = RetryConfig.default[IO]().copy(
    *       calcRetryDelay = ...
    *     )
    *   }}}
    * Note2: exceptions thrown in this partial function will cause the retry loop to fail immediately
    *
    * @param f a function that calculates the delay between retries
    * @return a new RetryPlan
    */
  def calcRetryDelay( f: PartialFunction[( RetryState, Throwable ), F[FiniteDuration]] ): RetryPlan[F] =
    copy( maybePfCalcRetryDelay = Some( f ) )

  /**
    * Copy a RetryPlan and set the function for handling retry events (overrides baseRetryConfig)
    * Note: exceptions thrown in the event handler will cause the retry loop to fail immediately
    * @param f a function that is called on each retry event
    * @return a new RetryPlan
    */
  def onRetryEvent( f: RetryEvent => F[Unit] ): RetryPlan[F] =
    copy( maybeOnRetryEvent = Some( f ) )

  /**
    * Copy a RetryPlan and set the correlation id to a fixed value (overrides baseRetryConfig)
    * Note: to replace the base correlation id generator, modify the implicit RetryConfig in scope, e.g.:
    *  {{{
    *    implicit val myRetryConfig: RetryConfig[IO] = RetryConfig.default[IO].copy(
    *      genCorrelationId = ...
    *    )
    *  }}}
    * @param id the correlation id
    * @return a new RetryPlan
    */
  def correlationId( id: String ): RetryPlan[F] =
    copy( maybeCorrelationId = Some( id ) )

  /**
    * @return a copy of the retry plan with the new base retry config set
    */
  def withBaseRetryConfig( newBaseRetryConfig: RetryConfig[F] ): RetryPlan[F] =
    copy()( baseRetryConfig = newBaseRetryConfig )

  /**
    * @return a new RetryConfig with the settings from the RetryPlan overriding the base RetryConfig
    */
  def overrideBaseRetryConfig( implicit monadF: Monad[F] ): RetryConfig[F] = RetryConfig(
    shouldRetry = maybePfShouldRetry match {
      case None => baseRetryConfig.shouldRetry
      case Some( pfShouldRetry ) => { ( retryState, ex ) =>
        pfShouldRetry.applyOrElse( ( retryState, ex ), baseRetryConfig.shouldRetry.tupled )
      }
    },
    calcRetryDelay = maybePfCalcRetryDelay match {
      case None => baseRetryConfig.calcRetryDelay
      case Some( pfCalcRetryDelay ) => { ( retryState, ex ) =>
        pfCalcRetryDelay.applyOrElse( ( retryState, ex ), baseRetryConfig.calcRetryDelay.tupled )
      }
    },
    onRetryEvent = maybeOnRetryEvent.getOrElse( baseRetryConfig.onRetryEvent ),
    genCorrelationId = { () =>
      maybeCorrelationId match {
        case Some( correlationId ) => Monad[F].pure( correlationId )
        case None                  => baseRetryConfig.genCorrelationId()
      }
    }
  )

  /**
    * Run the retry plan with a function that requires the current RetryState
    * @param f the function to run for each attempt
    * @param monadF the monad typeclass
    * @param sleepF the sleep typeclass
    * @param clockF the clock typeclass
    * @tparam A the return type of the attempt function
    * @return either the result or an exception
    */
  def runRetryState[A](
      f: RetryState => F[Either[Throwable, A]]
  )(
      implicit
      monadF: Monad[F],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[Either[Throwable, A]] = {
    val retryConfig: RetryConfig[F] = overrideBaseRetryConfig

    retryConfig.genCorrelationId().flatMap { correlationId =>
      clockF.realTimeInstant.flatMap { implicit now =>
        RetryCore
          .runRetryState[F, RetryState, A](
            onBeforeAttempt = { retryState =>
              clockF.realTimeInstant.flatMap { implicit now => // shadowed
                retryState.onBeforeAttempt( monadF, now )
              }
            },
            onAfterFailure = { ( retryState, cause ) =>
              clockF.realTimeInstant.flatMap { implicit now => // shadowed
                retryState.onAfterFailure( cause )
              }
            }
          )(
            initialState = RetryState.initial(
              maxAttempts = maxAttempts,
              maybeMaxElapsedDuration = maybeMaxElapsedDuration,
              correlationId = correlationId
            )
          )( f )
          .map( _._2 )
      }
    }
  }

  /**
    * Run the retry plan for a monadic context that supports capturing exceptions (e.g. IO, Future or Try)
    * @param f the function to run for each attempt
    * @param monadErrorF the monad error typeclass
    * @param sleepF the sleep typeclass
    * @param clockF the clock typeclass
    * @tparam A the return type of the attempt function
    * @return either the result or an exception in the monadic context
    */
  def run[A](
      f: () => F[A]
  )(
      implicit
      monadErrorF: MonadError[F, Throwable],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[A] = runRetryState( _ => f().attempt ).rethrow

  def apply[A](
      f: () => F[A]
  )(
      implicit
      monadErrorF: MonadError[F, Throwable],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[A] = run( f )

  /**
    * Run the retry plan for a monadic context that does not support capturing exceptions (e.g. cats.Id)
    *
    * @param f the function to run for each attempt (note: the function should not throw but instead return exceptions
    *          as Left(exception)
    * @param monadF the monad typeclass
    * @param sleepF the sleep typeclass
    * @param clockF  the clock typeclass
    * @tparam A the return type of the attempt function
    * @return either the result or an exception
    */
  def runCapture[A](
      f: () => F[Either[Throwable, A]]
  )(
      implicit
      monadF: Monad[F],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[Either[Throwable, A]] = runRetryState( _ => f() )

  def apply[A](
      f: () => F[Either[Throwable, A]]
  )(
      implicit
      monadF: Monad[F],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[Either[Throwable, A]] = runCapture( f )
}

object RetryPlan {

  /**
    * Creates a RetryPlan with no max attempts and the given maxElapsedDuration
    * Note: must supply a function to calculate the delay between retries since default exponential backoff
    * does not make sense for a retry plan with no max attempts
    *
    * @param maxElapsedDuration maximum elapsed time since start of retry call to run another attempt
    * @param calcRetryDelay function to compute the delay between retries
    * @return a RetryPlan
    */
  def apply[F[_]: Applicative](
      maxElapsedDuration: FiniteDuration,
      calcRetryDelay: ( RetryState, Throwable ) => F[FiniteDuration]
  )(
      implicit
      retryConfig: RetryConfig[F]
  ): RetryPlan[F] =
    RetryPlan(
      maxAttempts = Int.MaxValue,
      maybeMaxElapsedDuration = Some( maxElapsedDuration )
    )(
      retryConfig.copy(
        calcRetryDelay = calcRetryDelay
      )
    )

  /**
    * Creates a RetryPlan with no max attempts and the given maxElapsedDuration and a fixed retry interval
    * @param maxElapsedDuration maximum elapsed time since start of retry call to run another attempt
    * @param retryInterval the interval between retries
    * @return a RetryPlan
    */
  def apply[F[_]: Applicative](
      maxElapsedDuration: FiniteDuration,
      retryInterval: FiniteDuration
  )(
      implicit
      retryConfig: RetryConfig[F]
  ): RetryPlan[F] =
    RetryPlan( maxElapsedDuration, ( _, _ ) => retryInterval.pure[F] )

}
