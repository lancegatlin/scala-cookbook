package org.ldg.retry

import cats.implicits.toFunctorOps
import cats.{Applicative, Monad}

import java.time.Instant
import scala.concurrent.duration.{DurationLong, FiniteDuration}

/**
 * Represents the state of a retry operation
 * @param correlationId the correlation id (for logging)
 * @param attemptCount the number of attempts made so far
 * @param maxAttempts the maximum number of attempts allowed (Int.MaxValue for unlimited)
 * @param startTime the time when the retry operation started
 * @param maybeMaxElapsedTime the maximum elapsed time allowed for the retry operation (None for unlimited)
 * @param lastModified the last time the state was modified
 * @param maybeLastRetryEvent the last retry event (if any)
 */
case class RetryState(
  correlationId: String,
  attemptCount: Int,
  maxAttempts: Int,
  startTime: Instant,
  maybeMaxElapsedTime: Option[FiniteDuration],
  lastModified: Instant,
  maybeLastRetryEvent: Option[RetryEvent] = None
 ) {
  /**
   * @return the elapsed time between the start of the retry operation and the last modified time
   */
  def lastModifiedElapsed: FiniteDuration = java.time.Duration.between(startTime, lastModified).toNanos.nanos

  /**
   * Returns the elapsed time since the start of the retry operation
   * @param now the current time
   * @return elapsed time
   */
  def elapsed(implicit now: Instant): FiniteDuration = java.time.Duration.between(startTime, now).toNanos.nanos

  /**
   * Called before an attempt is made
   * @param now the current time
   * @return the updated state
   */
  def onBeforeAttempt[F[_]:Monad](implicit now: Instant): F[RetryState] = {
    val newState = copy(
      attemptCount = attemptCount + 1,
      lastModified = now
    )
    Applicative[F].pure(newState)
  }

  /**
   * Called after a failure
   * @param cause the cause of the failure
   * @param retryConfig the retry configuration
   * @param now the current time
   * @tparam F the monadic context (e.g. Future, IO, etc)
   * @return a tuple containing the updated state and an optional delay before retrying
   */
  def onAfterFailure[F[_]:Monad](cause: Throwable)(implicit retryConfig: RetryConfig[F], now: Instant): F[(RetryState, Option[FiniteDuration])] = {
    import RetryEvent._
    val ultimateCause = RetryCore.recurseUntilUltimateCause(cause)

    if (maybeMaxElapsedTime.forall(this.elapsed < _)) {
      if (retryConfig.shouldRetry(ultimateCause)) {
        if (attemptCount < maxAttempts) {
          val retryDelay = retryConfig.calcRetryDelay(this, cause)
          val retryAction = RetryAction.AttemptAgain(retryDelay)
          val retryEvent = OnAfterFailure(this, cause, retryAction)
          val newState = this.copy(
            maybeLastRetryEvent = Some(retryEvent),
            lastModified = now
          )
          retryConfig.onRetryEvent(retryEvent).as(
            (newState, Some(retryDelay))
          )
        } else {
          val retryAction = RetryAction.StopRetry(FailedRetryReason.ExhaustedMaxAttempts)
          val retryEvent = RetryEvent.OnAfterFailure(this, cause, retryAction)
          val newState = this.copy(
            maybeLastRetryEvent = Some(retryEvent),
            lastModified = now
          )
          retryConfig.onRetryEvent(retryEvent).as(
            (newState, None)
          )
        }
      } else {
        val retryAction = RetryAction.StopRetry(FailedRetryReason.UnretryableFailure)
        val retryEvent = RetryEvent.OnAfterFailure(this, cause, retryAction)
        val newState = this.copy(
          maybeLastRetryEvent = Some(retryEvent),
          lastModified = now
        )
        retryConfig.onRetryEvent(retryEvent).as(
          (newState, None)
        )
      }
    } else {
      val retryAction = RetryAction.StopRetry(FailedRetryReason.ExceededMaxElapsedTime)
      val retryEvent =RetryEvent.OnAfterFailure(this, cause, retryAction)
      val newState = this.copy(
        maybeLastRetryEvent = Some(retryEvent),
        lastModified = now
      )
      retryConfig.onRetryEvent(retryEvent).as(
        (newState, None)
      )
    }
  }

  /**
   * Called when a retry is successful
   * @param retryConfig the retry configuration
   * @param now the current time
   * @tparam F the monadic context (e.g. Future, IO, etc)
   * @return the updated state
   */
  def onRetrySuccess[F[_]:Monad]()(implicit retryConfig: RetryConfig[F], now: Instant): F[RetryState] = {
    val newState = copy(
      lastModified = now
    )

    for {
      _ <- if(attemptCount > 1) {
        retryConfig.onRetryEvent(RetryEvent.OnRetrySuccess(newState))
      } else {
        Applicative[F].unit
      }
    } yield newState
  }
}

  object RetryState {
    /**
     * Creates an initial retry state
     * @param maxAttempts the maximum number of attempts allowed (Int.MaxValue for unlimited)
     * @param maybeMaxElapsedTime the maximum elapsed time allowed for the retry operation (None for unlimited)
     * @param correlationId the correlation id (for logging)
     * @param now the current time
     * @return the initial retry state
     */
    def initial(
      maxAttempts: Int,
      maybeMaxElapsedTime: Option[FiniteDuration],
      correlationId: String
    )(implicit now: Instant) : RetryState = {
      RetryState(
        correlationId = correlationId,
        attemptCount = 0,
        maxAttempts = maxAttempts,
        startTime = now,
        maybeMaxElapsedTime = maybeMaxElapsedTime,
        lastModified = now
      )
    }

    /**
     * Formats the retry state for logging
     * @param retryState the retry state
     * @return the formatted string
     */
    def fmtRetryState(retryState: RetryState): String = {
      import retryState._
      val sMaxAttempts = if(maxAttempts == Int.MaxValue) "inf" else maxAttempts.toString
      val sMaxElapsedTime = maybeMaxElapsedTime.map(_.toMillis.millis).getOrElse("inf")
      s"attempts=$attemptCount/$sMaxAttempts elapsed=${lastModifiedElapsed.toMillis.millis}/$sMaxElapsedTime correlationId=$correlationId"
    }
  }
