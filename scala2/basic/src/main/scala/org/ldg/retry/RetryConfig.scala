package org.ldg.retry

import cats.Applicative
import cats.effect.std.Console
import cats.implicits.catsSyntaxApplicativeId
import org.ldg.effect.CaptureOrRunEffect
import org.slf4j.{Logger, LoggerFactory}
import org.typelevel.log4cats.{Logger => CatsLogger}

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration for retrying a function
  * @param shouldRetry a function for deciding if an exception should be retried
  * @param calcRetryDelay a function for calculating the delay before retrying
  * @param onRetryEvent a function for handling retry events
  * @param genCorrelationId a function for generating a correlation ID for the retry
  * @tparam F the monadic context (e.g. Future, IO, etc)
  */
case class RetryConfig[F[_]](
    shouldRetry: ( RetryState, Throwable ) => F[Boolean],
    calcRetryDelay: ( RetryState, Throwable ) => F[FiniteDuration],
    onRetryEvent: RetryEvent => F[Unit],
    genCorrelationId: () => F[String]
)

object RetryConfig {
  def default[F[_]: Applicative](
      maybeOnRetryEvent: Option[RetryEvent => F[Unit]] = None
  ): RetryConfig[F] = RetryConfig(
    shouldRetry = { ( _, ex ) =>
      RetryCore.defaultShouldRetry( ex ).pure
    },
    calcRetryDelay = { ( retryState, _ ) =>
      Applicative[F].pure(
        RetryCore.defaultRetryDelayWithExponentialBackoffAndJitter( retryState.attemptCount )
      )
    },
    onRetryEvent = maybeOnRetryEvent.getOrElse( { _ =>
      Applicative[F].unit
    } ),
    genCorrelationId = () => Applicative[F].pure( RetryCore.defaultGenCorrelationId() )
  )

  object NoLogging {
    def default[F[_]: Applicative](): RetryConfig[F] = RetryConfig.default()

    object Implicits {
      implicit def default[F[_]: Applicative]: RetryConfig[F] = RetryConfig.NoLogging.default()
    }
  }

  object Sl4jLogging {
    // note: use RetryConfig as default logger if user does not provide one (implicit or explicit)
    lazy val defaultSl4jLogger: Logger = LoggerFactory.getLogger( getClass.getName )

    def default[F[_]: Applicative: CaptureOrRunEffect]()(
        implicit
        logger: org.slf4j.Logger = defaultSl4jLogger ): RetryConfig[F] =
      RetryConfig.default( Some( RetryEventHandlers.slf4jLogEvent[F]( logger ) ) )

    object Implicits {
      implicit def default[F[_]: Applicative: CaptureOrRunEffect](
          implicit
          logger: org.slf4j.Logger = defaultSl4jLogger ): RetryConfig[F] = RetryConfig.Sl4jLogging.default()
    }
  }

  object CatsLogging {
    def default[F[_]: Applicative: CatsLogger](): RetryConfig[F] =
      RetryConfig.default( Some( RetryEventHandlers.catsLogging() ) )

    object Implicits {
      implicit def default[F[_]: Applicative: CatsLogger]: RetryConfig[F] =
        RetryConfig.CatsLogging.default()
    }
  }

  object CatsConsoleLogging {
    def default[F[_]: Applicative: Console](): RetryConfig[F] =
      RetryConfig.default( Some( RetryEventHandlers.catsConsoleLogging() ) )

    object Implicits {
      implicit def default[F[_]: Applicative: Console]: RetryConfig[F] =
        RetryConfig.CatsConsoleLogging.default()
    }
  }

  object StderrLogging {
    def default[F[_]: Applicative: CaptureOrRunEffect](): RetryConfig[F] =
      RetryConfig.default( Some( RetryEventHandlers.stderrLogging() ) )

    object Implicits {
      implicit def default[F[_]: Applicative: CaptureOrRunEffect]: RetryConfig[F] =
        RetryConfig.StderrLogging.default()
    }
  }
}
