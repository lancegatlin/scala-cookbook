package org.ldg.retry

import cats.effect.std.Console
import org.typelevel.log4cats.{Logger => CatsLogger}
import org.ldg.effect.CaptureOrRunEffect
import org.ldg.retry.RetryEvent.{OnAfterFailure, OnRetrySuccess}
import org.slf4j.{Logger => Slf4jLogger}
import org.slf4j.event.{Level => Slf4jLevel}

import java.io.PrintStream

object RetryEventHandlers {
  def retryEventToLogMessage( fmtRetryState: RetryState => String ): RetryEvent => ( Slf4jLevel, String ) = {
    case OnAfterFailure( retryState, cause, RetryAction.AttemptAgain( retryDelay ) ) =>
      ( Slf4jLevel.INFO, s"Retrying failure (${fmtRetryState( retryState )}) after $retryDelay delay: $cause" )
    case OnAfterFailure( retryState, cause, RetryAction.StopRetry( reason ) ) =>
      ( Slf4jLevel.WARN, s"Retry $reason (${fmtRetryState( retryState )}) rethrowing: $cause" )
    case OnRetrySuccess( retryState ) =>
      ( Slf4jLevel.INFO, s"Retry success (${fmtRetryState( retryState )})" )
  }

  private def errorlnLogging[F[_]](
      errorln: String => F[Unit],
      fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] = { retryEvent =>
    val ( level, msg ) = retryEventToLogMessage( fmtRetryState )( retryEvent )
    errorln( s"[$level]: $msg" )
  }

  def stderrLogging[F[_]: CaptureOrRunEffect](
      stderr: PrintStream = System.err,
      fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] = {
    val errorln: String => F[Unit] = msg =>
      CaptureOrRunEffect[F].effect(
        // scalastyle:off
        stderr.println( msg )
        // scalastyle:on
      )
    errorlnLogging( errorln, fmtRetryState )
  }

  def catsConsoleLogging[F[_]: Console](
      fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] =
    errorlnLogging(
      msg => Console[F].errorln( msg ),
      fmtRetryState
    )

  def catsLogging[F[_]: CatsLogger](
      fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] = { retryEvent =>
    val maybeThrowable = retryEvent match {
      case OnAfterFailure( _, cause, _ ) => Some( cause )
      case _                             => None
    }
    val ( level, msg ) = retryEventToLogMessage( fmtRetryState )( retryEvent )
    val logger = implicitly[CatsLogger[F]]
    level match {
      case Slf4jLevel.ERROR => maybeThrowable.fold( logger.error( msg ) )( logger.error( _ )( msg ) )
      case Slf4jLevel.WARN  => maybeThrowable.fold( logger.warn( msg ) )( logger.warn( _ )( msg ) )
      case Slf4jLevel.INFO  => maybeThrowable.fold( logger.info( msg ) )( logger.info( _ )( msg ) )
      case Slf4jLevel.DEBUG => maybeThrowable.fold( logger.debug( msg ) )( logger.debug( _ )( msg ) )
      case Slf4jLevel.TRACE => maybeThrowable.fold( logger.trace( msg ) )( logger.trace( _ )( msg ) )
    }
  }

  def slf4jLogEvent[F[_]: CaptureOrRunEffect](
      logger: Slf4jLogger,
      fmtRetryState: RetryState => String = RetryState.fmtRetryState
  ): RetryEvent => F[Unit] = { retryEvent =>
    val maybeThrowable = retryEvent match {
      case OnAfterFailure( _, cause, _ ) => Some( cause )
      case _                             => None
    }
    val ( level, msg ) = retryEventToLogMessage( fmtRetryState )( retryEvent )
    CaptureOrRunEffect[F].effect(
      level match {
        case Slf4jLevel.ERROR => maybeThrowable.fold( logger.error( msg ) )( logger.error( msg, _ ) )
        case Slf4jLevel.WARN  => maybeThrowable.fold( logger.warn( msg ) )( logger.warn( msg, _ ) )
        case Slf4jLevel.INFO  => maybeThrowable.fold( logger.info( msg ) )( logger.info( msg, _ ) )
        case Slf4jLevel.DEBUG => maybeThrowable.fold( logger.debug( msg ) )( logger.debug( msg, _ ) )
        case Slf4jLevel.TRACE => maybeThrowable.fold( logger.trace( msg ) )( logger.trace( msg, _ ) )
      }
    )
  }
}