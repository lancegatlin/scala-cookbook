package org.ldg.retry

import cats.Applicative

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
  shouldRetry: Throwable => Boolean,
  calcRetryDelay: (RetryState, Throwable) => FiniteDuration,
  onRetryEvent: RetryEvent => F[Unit],
  genCorrelationId: () => String
)

object RetryConfig {
  implicit def default[F[_]:Applicative]: RetryConfig[F] = RetryConfig(
    shouldRetry = RetryCore.defaultShouldRetry,
    calcRetryDelay = { (retryState, _) =>
      RetryCore.defaultRetryDelayWithExponentialBackoffAndJitter(retryState.attemptCount)
    },
    onRetryEvent = { _ => Applicative[F].unit },
    genCorrelationId = RetryCore.defaultGenCorrelationId
  )
}
