package org.ldg.util.retry

import cats.effect.kernel.Sync
import cats.effect.std.Console
import org.ldg.util.retry.RetryEvent.{OnAfterFailure, OnRetrySuccess}
import org.slf4j.Logger
import org.slf4j.event.Level

object RetryEventHandlers {
  def retryEventToLogMessage(fmtRetryState: RetryState => String): RetryEvent => (Level, String) = {
    case OnAfterFailure(retryState, cause, RetryAction.AttemptAgain(retryDelay)) =>
      (Level.INFO, s"Retrying failure (${fmtRetryState(retryState)}) after $retryDelay delay: $cause")
    case OnAfterFailure(retryState, cause, RetryAction.StopRetry(reason))  =>
      (Level.WARN, s"Retry $reason (${fmtRetryState(retryState)}) rethrowing: $cause")
    case OnRetrySuccess(retryState) =>
      (Level.INFO, s"Retry success (${fmtRetryState(retryState)})")
  }

  def logByLevel(logger: Logger)(level: Level, msg: String): Unit =
    level match {
      case Level.WARN => logger.warn(msg)
      case Level.ERROR => logger.error(msg)
      case Level.INFO => logger.info(msg)
      case Level.DEBUG => logger.debug(msg)
      case Level.TRACE => logger.trace(msg)
    }

  def stderrLogEvent[F[_]:Console](
    logger: Logger,
    fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] = { retryEvent =>
    val (level, msg) = retryEventToLogMessage(fmtRetryState)(retryEvent)
    Console[F].errorln(s"[$level]: $msg")
  }

  def slf4jLogEvent[F[_]:Sync](
    logger: Logger,
    fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] = {
    val log = logByLevel(logger) _

    { retryEvent: RetryEvent =>
      val (level, msg) = retryEventToLogMessage(fmtRetryState)(retryEvent)
      Sync[F].delay(log(level, msg))
    }
  }
}
