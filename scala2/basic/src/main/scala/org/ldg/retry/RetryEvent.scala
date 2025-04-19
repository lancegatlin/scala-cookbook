package org.ldg.retry

import scala.concurrent.duration.FiniteDuration

sealed trait FailedRetryReason
object FailedRetryReason {
  case object UnretryableFailure extends FailedRetryReason
  case object ExhaustedMaxAttempts extends FailedRetryReason
  case object ExceededMaxElapsedTime extends FailedRetryReason
}

sealed trait RetryAction
object RetryAction {
  case class AttemptAgain(retryDelay: FiniteDuration) extends RetryAction
  case class StopRetry(reason: FailedRetryReason) extends RetryAction
}

sealed trait RetryEvent
object RetryEvent {
  case class OnAfterFailure(retryState: RetryState, cause: Throwable, action: RetryAction) extends RetryEvent
  // only occurs if there was at least one retry
  case class OnRetrySuccess(retryState: RetryState) extends RetryEvent
}

