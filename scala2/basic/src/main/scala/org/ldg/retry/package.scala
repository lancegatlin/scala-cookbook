package org.ldg

import cats.{Applicative, Monad, MonadError}
import cats.effect.Clock
import cats.implicits.catsSyntaxApplicativeId
import org.ldg.effect.Sleep

import scala.concurrent.duration.FiniteDuration

/**
  * Retry a function until it succeeds or exceeds max attempts or exceeds max elapsed time or throws an unretryable
  * exception
  *
  * Example of retrying a code block at most 5 times using default settings:
  * {{{
  *   val doEffect: String => Future[String] = ???
  *   retry(5)(() => doEffect("example"))
  * }}}
  * Example of retrying a code block for at most 1 minute using default settings:
  * {{{
  *   retry(1.minute)(() => doEffect("example"))
  * }}}
  * Example of retrying with custom configuration:
  * {{{
  *   RetryPlan(5)
  *     .shouldRetry {
  *       case ex:RuntimeException => false
  *     }
  *     .onRetryEvent(println)
  *     .calcRetryDelay((_,_) => 5.seconds)
  *     .run(() => doEffect("example"))
  * }}}
  * Example of wrapping an existing function with a saved RetryPlan:
  * {{{
  *   import Retry.Implicits._
  *   val retryPlan =
  *     RetryPlan(5)
  *       .shouldRetry {
  *         case _:RuntimeException => false
  *       }
  *       .onRetryEvent(println)
  *       .calcRetryDelay((_,_) => 5.seconds)
  *   val doEffectWithRetry: String => Future[String] = doEffect.withRetry(retryPlan)
  *   val doEffectWithRetry2: String => Future[String] = doEffect.withRetry(retry(5))
  * }}}
  */
package object retry {

  /**
    * Retry a function until it succeeds or exceeds max attempts or exceeds max elapsed time or throws an unretryable
    * exception.
    * @param maxAttempts maximum number of attempts
    * @param maybeMaxElapsedDuration maybe a maximum elapsed time for retries
    *                                Note: this is not a timeout and will not interrupt a retry in progress
    * @param f function to retry
    * @return the result of retrying the function in the monadic context F
    */
  def retry[F[_], A](
      maxAttempts: Int,
      maybeMaxElapsedDuration: Option[FiniteDuration] = None
  )(
      f: () => F[A]
  )(
      implicit
      retryConfig: RetryConfig[F],
      monadErrorF: MonadError[F, Throwable],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[A] = RetryPlan( maxAttempts, maybeMaxElapsedDuration ).run( f )

  /**
    * Retry a function until it succeeds or exceeds max attempts or exceeds max elapsed time or throws an unretryable
    * exception.
    * @param maxAttempts maximum number of attempts
    * @param maybeMaxElapsedDuration maybe a maximum elapsed time for retries
    *                                Note: this is not a timeout and will not interrupt a retry in progress
    * @param f function to retry
    * @return
    */
  def retryCapture[F[_], A](
      maxAttempts: Int,
      maybeMaxElapsedDuration: Option[FiniteDuration] = None
  )(
      f: () => F[Either[Throwable, A]]
  )(
      implicit
      retryConfig: RetryConfig[F],
      monad: Monad[F],
      sleepF: Sleep[F],
      clockF: Clock[F] ): F[Either[Throwable, A]] = RetryPlan( maxAttempts, maybeMaxElapsedDuration ).runCapture( f )

  /**
    * Retry a function until it succeeds or exceeds max elapsed time or throws an unretryable exception.
    * Note: must supply a function to calculate the delay between retries since default exponential backoff
    * without max attempts could result in very long delays between retries.
    *
    * @param maxElapsedDuration maximum elapsed time since start of retry call to run another attempt
    * @param calcRetryDelay function to compute the delay between retries
    * @param f function to retry
    * @return
    */
  def retry[F[_]: Applicative, A](
      maxElapsedDuration: FiniteDuration,
      calcRetryDelay: ( RetryState, Throwable ) => F[FiniteDuration]
  )(
      f: () => F[A]
  )(
      implicit
      retryConfig: RetryConfig[F],
      monadErrorF: MonadError[F, Throwable],
      sleepF: Sleep[F],
      clockF: Clock[F]
  ): F[A] =
    RetryPlan(
      maxElapsedDuration = maxElapsedDuration,
      calcRetryDelay = calcRetryDelay
    ).run( f )

  /**
    * Retry a function until it succeeds or exceeds max elapsed time or throws an unretryable exception.
    * Note: must supply a function to calculate the delay between retries since default exponential backoff
    * without max attempts could result in very long delays between retries.
    *
    * @param maxElapsedDuration maximum elapsed time since start of retry call to run another attempt
    * @param calcRetryDelay function to compute the delay between retries
    * @param f function to retry
    * @return
    */
  def retryCapture[F[_]: Applicative, A](
      maxElapsedDuration: FiniteDuration,
      calcRetryDelay: ( RetryState, Throwable ) => F[FiniteDuration]
  )(
      f: () => F[Either[Throwable, A]]
  )(
      implicit
      retryConfig: RetryConfig[F],
      monadErrorF: MonadError[F, Throwable],
      sleepF: Sleep[F],
      clockF: Clock[F]
  ): F[Either[Throwable, A]] =
    RetryPlan(
      maxElapsedDuration = maxElapsedDuration,
      calcRetryDelay = calcRetryDelay
    ).runCapture( f )

  /**
    * Creates a RetryPlan with no max attempts and the given maxElapsedDuration and a fixed retry interval
    * @param maxElapsedDuration maximum elapsed time since start of retry call to run another attempt
    * @param retryInterval the interval between retries
    * @return
    */
  def retry[F[_], A](
      maxElapsedDuration: FiniteDuration,
      retryInterval: FiniteDuration
  )(
      f: () => F[A]
  )(
      implicit
      retryConfig: RetryConfig[F],
      monadErrorF: MonadError[F, Throwable],
      sleepF: Sleep[F],
      clockF: Clock[F]
  ): F[A] = RetryPlan( maxElapsedDuration, ( _, _ ) => retryInterval.pure[F] ).run( f )

  /**
    * Creates a RetryPlan with no max attempts and the given maxElapsedDuration and a fixed retry interval
    * @param maxElapsedDuration maximum elapsed time since start of retry call to run another attempt
    * @param retryInterval the interval between retries
    * @return
    */
  def retry[F[_], A](
      maxElapsedDuration: FiniteDuration,
      retryInterval: FiniteDuration
  )(
      f: () => F[Either[Throwable, A]]
  )(
      implicit
      retryConfig: RetryConfig[F],
      monad: Monad[F],
      sleepF: Sleep[F],
      clockF: Clock[F]
  ): F[Either[Throwable, A]] = RetryPlan( maxElapsedDuration, ( _, _ ) => retryInterval.pure[F] ).runCapture( f )

  // Implicit helpers

  /**
    * Implicit classes to wrap retry functionality around a function that takes one or more arguments
    * Example:
    *   {{{
    *     val doEffect: String => Future[String] = ???
    *     val doEffectWithRetry: String => Future[String] = doEffect.withRetry(retry(5))
    *     val myRetryPlan = retry(3).shouldRetry {
    *       case _:RuntimeException => false
    *     }
    *     val doEffectWithRetry2: String => Future[String] = doEffect.withRetry(myRetryPlan)
    *   }}}
    */
  implicit class RetryFunctionExt[M[_], A, R](
      val self: A => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): A => M[R] = { a =>
      retryPlan( () => self( a ) )
    }
  }

  implicit class RetryFunction2Ext[M[_], A, B, R](
      val self: ( A, B ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B ) => M[R] = { ( a, b ) =>
      retryPlan( () => self( a, b ) )
    }
  }

  implicit class RetryFunction3Ext[M[_], A, B, C, R](
      val self: ( A, B, C ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C ) => M[R] = { ( a, b, c ) =>
      retryPlan( () => self( a, b, c ) )
    }
  }

  implicit class RetryFunction4Ext[M[_], A, B, C, D, R](
      val self: ( A, B, C, D ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C, D ) => M[R] = { ( a, b, c, d ) =>
      retryPlan( () => self( a, b, c, d ) )
    }
  }

  implicit class RetryFunction5Ext[M[_], A, B, C, D, E, R](
      val self: ( A, B, C, D, E ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C, D, E ) => M[R] = { ( a, b, c, d, e ) =>
      retryPlan( () => self( a, b, c, d, e ) )
    }
  }

  implicit class RetryFunction6Ext[M[_], A, B, C, D, E, F, R](
      val self: ( A, B, C, D, E, F ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C, D, E, F ) => M[R] = { ( a, b, c, d, e, f ) =>
      retryPlan( () => self( a, b, c, d, e, f ) )
    }
  }

  implicit class RetryFunction7Ext[M[_], A, B, C, D, E, F, G, R](
      val self: ( A, B, C, D, E, F, G ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C, D, E, F, G ) => M[R] = { ( a, b, c, d, e, f, g ) =>
      retryPlan( () => self( a, b, c, d, e, f, g ) )
    }
  }

  implicit class RetryFunction8Ext[M[_], A, B, C, D, E, F, G, H, R](
      val self: ( A, B, C, D, E, F, G, H ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C, D, E, F, G, H ) => M[R] = { ( a, b, c, d, e, f, g, h ) =>
      retryPlan( () => self( a, b, c, d, e, f, g, h ) )
    }
  }

  implicit class RetryFunction9Ext[M[_], A, B, C, D, E, F, G, H, I, R](
      val self: ( A, B, C, D, E, F, G, H, I ) => M[R]
  ) extends AnyVal {
    def withRetry(
        retryPlan: RetryPlan[M]
    )(
        implicit
        monadErrorM: MonadError[M, Throwable],
        sleepM: Sleep[M],
        clockM: Clock[M] ): ( A, B, C, D, E, F, G, H, I ) => M[R] = { ( a, b, c, d, e, f, g, h, i ) =>
      retryPlan( () => self( a, b, c, d, e, f, g, h, i ) )
    }
  }
}
