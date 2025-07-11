package org.ldg.retry

import cats.implicits.{toFlatMapOps, toFunctorOps}
import cats.{Applicative, Monad}

import java.time.Instant
import scala.concurrent.duration.{DurationLong, FiniteDuration}

/**
  * Represents the state of a retry operation
  * @param correlationId the correlation id (for logging)
  * @param attemptCount the number of attempts made so far
  * @param maxAttempts the maximum number of attempts allowed (Int.MaxValue for unlimited)
  * @param startTime the time when the retry operation started
  * @param maybeMaxElapsedDuration the maximum elapsed time allowed for the retry operation (None for unlimited)
  * @param lastModified the last time the state was modified
  * @param maybeLastRetryEvent the last retry event (if any)
  */
case class RetryState(
    correlationId: String,
    attemptCount: Int,
    maxAttempts: Int,
    startTime: Instant,
    maybeMaxElapsedDuration: Option[FiniteDuration],
    lastModified: Instant,
    maybeLastRetryEvent: Option[RetryEvent] = None
) {

  def maybeMaxElapsedInstant: Option[Instant] =
    maybeMaxElapsedDuration.map { maxElapsedDuration =>
      startTime.plusNanos( maxElapsedDuration.toNanos )
    }

  /**
    * @return the elapsed time between the start of the retry operation and the last modified time
    */
  def lastModifiedElapsed: FiniteDuration = java.time.Duration.between( startTime, lastModified ).toNanos.nanos

  /**
    * Returns the elapsed time since the start of the retry operation
    * @param now the current time
    * @return elapsed time
    */
  def elapsed( implicit now: Instant ): FiniteDuration = java.time.Duration.between( startTime, now ).toNanos.nanos

  /**
    * Transition this state to a new state before an attempt is made
    * @param now the current time
    * @return the updated state
    */
  def onBeforeAttempt[F[_]: Monad]( implicit now: Instant ): F[RetryState] = {
    val newState = copy(
      attemptCount = attemptCount + 1,
      lastModified = now
    )
    Applicative[F].pure( newState )
  }

  private def onAfterFailure[F[_]: Monad](
      cause: Throwable,
      retryAction: RetryAction
  )(
      implicit
      retryConfig: RetryConfig[F],
      now: Instant ): F[( RetryState, Option[FiniteDuration] )] = {
    val retryEvent = RetryEvent.OnAfterFailure( this, cause, retryAction )
    val newState = this.copy(
      maybeLastRetryEvent = Some( retryEvent ),
      lastModified = now
    )
    val maybeRetryDelay = retryAction match {
      case RetryAction.AttemptAgain( retryDelay ) => Some( retryDelay )
      case _                                      => None
    }
    retryConfig
      .onRetryEvent( retryEvent ).as(
        ( newState, maybeRetryDelay )
      )
  }

  /**
    * Transition this state to a new state after a failure
    * @param cause the cause of the failure
    * @param retryConfig the retry configuration
    * @param now the current time
    * @tparam F the monadic context (e.g. Future, IO, etc)
    * @return a tuple containing the updated state and an optional delay before retrying
    */
  def onAfterFailure[F[_]: Monad](
      cause: Throwable
  )(
      implicit
      retryConfig: RetryConfig[F],
      now: Instant ): F[( RetryState, Option[FiniteDuration] )] = {
    import RetryAction._
    import FailedRetryReason._

    val ultimateCause = RetryCore.recurseUntilUltimateCause( cause )

    if (maybeMaxElapsedDuration.forall( this.elapsed < _ )) {
      retryConfig.shouldRetry( this, ultimateCause ).flatMap { shouldRetry =>
        if (shouldRetry) {
          if (attemptCount < maxAttempts) {
            retryConfig.calcRetryDelay( this, cause ).flatMap { retryDelay =>
              maybeMaxElapsedInstant match {
                case Some( maxElapsedInstant ) =>
                  // check if the retry delay exceeds the max elapsed time
                  val afterRetryDelayInstant = now.plusNanos( retryDelay.toNanos )
                  if (afterRetryDelayInstant.isBefore( maxElapsedInstant )) {
                    onAfterFailure( cause, AttemptAgain( retryDelay ) )
                  } else {
                    onAfterFailure( cause, StopRetry( ExceededMaxElapsedDuration ) )
                  }
                case _ => onAfterFailure( cause, AttemptAgain( retryDelay ) )
              }

            }
          } else {
            onAfterFailure( cause, StopRetry( ExhaustedMaxAttempts ) )
          }
        } else {
          onAfterFailure( cause, StopRetry( UnretryableFailure ) )
        }
      }
    } else {
      onAfterFailure( cause, StopRetry( ExceededMaxElapsedDuration ) )
    }
  }

  /**
    * Transition this state to a new state after an attempt succeeds
    * @param retryConfig the retry configuration
    * @param now the current time
    * @tparam F the monadic context (e.g. Future, IO, etc)
    * @return the updated state
    */
  def onAfterAttemptSuccess[F[_]: Monad]()( implicit retryConfig: RetryConfig[F], now: Instant ): F[RetryState] = {
    val newState = copy(
      lastModified = now
    )

    for {
      // no RetryEvents are emitted if first attempt succeeds
      _ <- if (attemptCount > 1) {
        retryConfig.onRetryEvent( RetryEvent.OnRetrySuccess( newState ) )
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
    * @param maybeMaxElapsedDuration the maximum elapsed time allowed for the retry operation (None for unlimited)
    * @param correlationId the correlation id (for logging)
    * @param now the current time
    * @return the initial retry state
    */
  def initial(
      maxAttempts: Int,
      maybeMaxElapsedDuration: Option[FiniteDuration],
      correlationId: String
  )( implicit now: Instant ): RetryState =
    RetryState(
      correlationId = correlationId,
      attemptCount = 0,
      maxAttempts = maxAttempts,
      startTime = now,
      maybeMaxElapsedDuration = maybeMaxElapsedDuration,
      lastModified = now
    )

  /**
    * Formats the retry state for logging
    * @param retryState the retry state
    * @return the formatted string
    */
  def fmtRetryState( retryState: RetryState ): String = {
    import retryState._
    val sMaxAttempts = if (maxAttempts == Int.MaxValue) "inf" else maxAttempts.toString
    val sMaxElapsedTime = maybeMaxElapsedDuration.map( _.toMillis.millis ).getOrElse( "inf" )
    s"attempts=$attemptCount/$sMaxAttempts elapsed=${lastModifiedElapsed.toMillis.millis}/$sMaxElapsedTime correlationId=$correlationId"
  }
}
