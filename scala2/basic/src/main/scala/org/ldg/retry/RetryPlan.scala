package org.ldg.retry

import cats.effect.kernel.Clock
import cats.implicits.{catsSyntaxApplicativeError, catsSyntaxMonadErrorRethrow, toFlatMapOps, toFunctorOps}
import cats.{Monad, MonadError}
import org.ldg.effect.Sleep

import scala.concurrent.duration.FiniteDuration

/**
 * A retry plan that defines the retry strategy for retrying an operation.
 *
 * @param maxAttempts the maximum number of attempts to retry (Int.MaxValue for unlimited)
 * @param maybeMaxElapsedTime maybe a maximum elapsed time for retries
 * @param maybePartialShouldRetry a partial function that defines which exceptions should be retried
 * @param maybeRetryDelay a function that calculates the delay between retries
 * @param maybeOnRetryEvent a function that is called on each retry event
 * @param maybeCorrelationId an optional correlation ID for the retry plan
 * @tparam F the monadic context (e.g. IO, Future, Id)
 */
case class RetryPlan[F[_]](
  maxAttempts: Int,
  maybeMaxElapsedTime: Option[FiniteDuration] = None,
  maybePartialShouldRetry: Option[PartialFunction[Throwable, Boolean]] = None,
  maybeRetryDelay: Option[(RetryState, Throwable) => FiniteDuration] = None,
  maybeOnRetryEvent: Option[RetryEvent => F[Unit]] = None,
  maybeCorrelationId: Option[String] = None
) {
  /**
   * Copy a RetryPlan and append a new partial function for deciding if an exception should be retried
   * Note: exceptions thrown in the function will cause the retry loop to fail immediately
   * Note: to replace the base shouldRetry handler, modify the implicit RetryConfig in scope, e.g.:
   *   {{{
   *     implicit val myRetryConfig: RetryConfig[IO] = RetryConfig.default[IO].copy(
   *       shouldRetry = ...
   *     )
   *   }}}
   * @param f a partial function that defines which exceptions should be retried
   * @return a new RetryPlan
   */
  def shouldRetry(f: PartialFunction[Throwable, Boolean]): RetryPlan[F] =
    copy(maybePartialShouldRetry = Some(f))

  /**
   * Copy a RetryPlan and append a new function for handling retry events
   * Note: exceptions thrown in the event handler will cause the retry loop to fail immediately
   * @param f a function that is called on each retry event
   * @return a new RetryPlan
   */
  def onRetryEvent(f: RetryEvent => F[Unit]): RetryPlan[F] =
    copy(maybeOnRetryEvent = Some(f))

  /**
   * Copy a RetryPlan and append a new function for calculating the delay between retries
   * Note: exceptions thrown in the function will cause the retry loop to fail immediately
   * @param f a function that calculates the delay between retries
   * @return a new RetryPlan
   */
  def retryDelay(f: (RetryState, Throwable) => FiniteDuration): RetryPlan[F] =
    copy(maybeRetryDelay = Some(f))

  /**
   * Copy a RetryPlan and set the correlation id
   * Note: to replace the base correlation id generator, modify the implicit RetryConfig in scope, e.g.:
   *  {{{
   *    implicit val myRetryConfig: RetryConfig[IO] = RetryConfig.default[IO].copy(
   *      genCorrelationId = ...
   *    )
   *  }}}
   * @param id the correlation id
   * @return a new RetryPlan
   */
  def correlationId(id: String): RetryPlan[F] =
    copy(maybeCorrelationId = Some(id))

  /**
   * Stack the RetryPlan settings on top of an implicit base RetryConfig in scope
   * @param baseRetryConfig the base retry configuration
   * @return a new RetryConfig with the settings from the RetryPlan overriding the base RetryConfig
   */
  def toRetryConfig(implicit baseRetryConfig: RetryConfig[F]): RetryConfig[F] = RetryConfig(
    shouldRetry = maybePartialShouldRetry match {
      case None => baseRetryConfig.shouldRetry
      case Some(partialShouldRetry) => { ex: Throwable => partialShouldRetry.applyOrElse(ex, baseRetryConfig.shouldRetry) }
    },
    calcRetryDelay = maybeRetryDelay.getOrElse(baseRetryConfig.calcRetryDelay),
    onRetryEvent = maybeOnRetryEvent.getOrElse(baseRetryConfig.onRetryEvent),
    genCorrelationId = { () => maybeCorrelationId.getOrElse(baseRetryConfig.genCorrelationId()) }
  )

  /**
   * Run the retry plan with a function that requires the current RetryState
   * @param f the function to run for each attempt
   * @param baseRetryConfig the base retry configuration
   * @param monadF the monad typeclass
   * @param sleepF the sleep typeclass
   * @param clockF the clock typeclass
   * @tparam A the return type of the attempt function
   * @return either the result or an exception
   */
  def runRetryState[A](
    f: RetryState => F[Either[Throwable, A]]
  )(implicit
    baseRetryConfig: RetryConfig[F],
    monadF: Monad[F],
    sleepF: Sleep[F],
    clockF: Clock[F]
  ): F[Either[Throwable, A]] = {
    val retryConfig: RetryConfig[F] = toRetryConfig(baseRetryConfig)

    clockF.realTimeInstant.flatMap { implicit now =>
      RetryCore.runRetryState[F, RetryState, A](
          onBeforeAttempt = { retryState =>
            clockF.realTimeInstant.flatMap { implicit now => // shadowed
              retryState.onBeforeAttempt(monadF, now)
            }
          },
          onAfterFailure = { (retryState, cause) =>
            clockF.realTimeInstant.flatMap { implicit now => // shadowed
              retryState.onAfterFailure(cause)
            }
          }
        )(
          initialState = RetryState.initial(
            maxAttempts = maxAttempts,
            maybeMaxElapsedTime = maybeMaxElapsedTime,
            correlationId = retryConfig.genCorrelationId()
          )
        )(f)
        .map(_._2)
    }
  }

  /**
   * Run the retry plan for a monadic context that supports capturing exceptions (e.g. IO, Future or Try)
   * @param f the function to run for each attempt
   * @param retryConfig the retry configuration
   * @param monadErrorF the monad error typeclass
   * @param sleepF the sleep typeclass
   * @param clockF the clock typeclass
   * @tparam A the return type of the attempt function
   * @return either the result or an exception in the monadic context
   */
  def run[A](
    f: => F[A]
  )(implicit
    retryConfig: RetryConfig[F],
    monadErrorF: MonadError[F, Throwable],
    sleepF: Sleep[F],
    clockF: Clock[F]
  ): F[A] = runRetryState(_ => f.attempt).rethrow

  def apply[A](
    f: => F[A]
  )(implicit
    retryConfig: RetryConfig[F],
    monadErrorF: MonadError[F, Throwable],
    sleepF: Sleep[F],
    clockF: Clock[F]
  ): F[A] = run(f)

  /**
   * Run the retry plan for a monadic context that does not support capturing exceptions (e.g. cats.Id)
   *
   * @param f the function to run for each attempt
   * @param retryConfig the retry configuration
   * @param monadF the monad typeclass
   * @param sleepF the sleep typeclass
   * @param clockF  the clock typeclass
   * @tparam A the return type of the attempt function
   * @return either the result or an exception
   */
  def runCapture[A](
    f: => F[Either[Throwable, A]]
  )(implicit
    retryConfig: RetryConfig[F],
    monadF: Monad[F],
    sleepF: Sleep[F],
    clockF: Clock[F]
  ): F[Either[Throwable, A]] = runRetryState(_ => f)

  def apply[A](
    f: => F[Either[Throwable, A]]
  )(implicit
    retryConfig: RetryConfig[F],
    monadF: Monad[F],
    sleepF: Sleep[F],
    clockF: Clock[F]
  ): F[Either[Throwable, A]] = runCapture(f)
}