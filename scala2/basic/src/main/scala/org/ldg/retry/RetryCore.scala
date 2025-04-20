package org.ldg.retry

import java.io.FileNotFoundException
import java.util.UUID
import scala.concurrent.duration.{Duration, DurationLong, FiniteDuration}
import scala.util.Random
import scala.util.control.NonFatal
import cats._
import cats.implicits.toFlatMapOps
import org.ldg.effect.Sleep

/**
 * Utility methods for retrying a function
 */
object RetryCore {
  def defaultGenCorrelationId(): String = UUID.randomUUID().toString

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

  /**
   * Calculate a retry delay with exponential backoff and jitter
   *
   * @param baseDelay base delay after failures which is exponentially increased after first failure
   * @param maxJitter amount of jitter to add to the computed delay
   * @param maxDelay maximum delay to return
   * @param attemptCount the number of attempts so far (1-based)
   * @return
   */
  def retryDelayWithExponentialBackoffAndJitter(
    baseDelay: FiniteDuration,
    maxJitter: FiniteDuration,
    maxDelay: FiniteDuration
  )(
    attemptCount: Int
  ): FiniteDuration = {
    require(attemptCount > 0)

    val attemptZeroCount = attemptCount - 1
    val limitedAttempts = Math.min(attemptZeroCount, RETRY_DELAY_ATTEMPT_COUNT_MAX)
    val initialDelayNs = baseDelay.toNanos

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
      baseDelay = 250.millis,
      maxJitter = 10.millis,
      // note: with initialDelay 250ms, retry 6+ (16s) limited to 10 seconds
      maxDelay = 10.seconds
    )

  /**
   * Recursively find the ultimate cause of a Throwable
   * @param cause the Throwable to check
   * @return the ultimate cause of the Throwable (if none found, return the original Throwable)
   */
  def recurseUntilUltimateCause(cause: Throwable): Throwable =
    Option(cause.getCause).fold(cause)(recurseUntilUltimateCause)

  /**
   * Run a retry loop with a stateful function for retry state
   *
   * @param onBeforeAttempt event handler for before each attempt (which can modify retry state)
   * @param onAfterFailure event handler for after each failure (which can modify retry state)
   * @param initialState initial retry state
   * @param attempt function to run for each attempt
   * @tparam F typeclass for effect; Note: cats.Id (or scala.util.Try) can be used here, but they are not recommended
   *           for production use (i.e. blocking) and for cats.Id any anonymous function passed should never throw
   *           directly (though attempt can still return a Left of an exception)
   * @tparam A return type of the attempt function
   * @tparam S type of the retry state
   * @return a tuple of the final retry state and the result of the attempt function
   */
  def runRetryState[F[_]:Monad:Sleep, S, A](
    onBeforeAttempt: S => F[S],
    onAfterFailure:  (S, Throwable) => F[(S, Option[FiniteDuration])]
  )(
    initialState: S
  )(
    attempt: S => F[Either[Throwable, A]]
  ): F[(S, Either[Throwable, A])] = {
    def attemptLoop(state0: S): F[(S, Either[Throwable, A])] =
      onBeforeAttempt(state0)
        .flatMap { state1 =>
          attempt(state1).flatMap {
            case success@Right(_) =>
              Applicative[F].pure((state1, success))
            // don't retry fatal exceptions
            case fatal@Left(cause) if NonFatal(recurseUntilUltimateCause(cause)) == false =>
              Applicative[F].pure((state1, fatal))
            case Left(cause) =>
              onAfterFailure(state1, cause).flatMap {
                case (state2, Some(Duration.Zero)) => attemptLoop(state2)
                case (state2, Some(retryDelay)) => Sleep[F].sleep(retryDelay).flatMap(_ => attemptLoop(state2))
                // no retry state, return last state and error to caller
                case (state2, None) => Applicative[F].pure((state2, Left(cause)))
              }
          }
        }
    attemptLoop(initialState)
  }

}
