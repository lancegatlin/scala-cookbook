package org.ldg

import cats.MonadError
import cats.effect.Clock
import org.ldg.effect.Sleep

import scala.concurrent.duration.FiniteDuration
/**
 * Retry a function until it succeeds or exceeds max attempts or exceeds max elapsed time or throws an unretryable
 * exception
 *
 * Example of retrying a code block at most 5 times using default settings:
 * {{{
 *   val doEffect: String => Future[String]
 *   retry(5)(doEffect("example"))
 * }}}
 * Example of retrying a code block for at most 1 minute using default settings:
 * {{{
 *   retry(1.minute)(doEffect("example"))
 * }}}
 * Example of retrying with configuration:
 * {{{
 * val logger: org.slf4j.Logger = ???
 *
 *   retry(5)
 *     .shouldRetry {
 *       case ex:RuntimeException => false
 *     }
 *     .onRetryEvent(RetryEventHandler.slf4jLogEvent(logger))
 *     .calcRetryDelay((_,_) => 5.seconds)
 *     .run(doEffect("example"))
 * }}}
 * Example of wrapping an existing function with a saved RetryPlan:
 * {{{
 *   import Retry.Implicits._
 *   val retryPlan =
 *     retry(5)
 *       .shouldRetry {
 *         case _:RuntimeException => false
 *       }
 *   val doEffectWithRetry: String => Future[String] = doEffect.withRetry(retryPlan)
 *   val doEffectWithRetry2: String => Future[String] = doEffect.withRetry(retry(5))
 * }}}
 */
package object retry {
  /**
   * Creates a RetryPlan with the given maxAttempts and maxElapsedTime
   * @param maxAttempts maximum number of attempts
   * @param maxElapsedTime maximum elapsed time since start of retry call to run another attempt (set to Duration.Inf
   *                       for no time limit) Note: this is not a timeout and will not interrupt a retry in progress
   * @return
   */
  def retry[F[_]](
     maxAttempts: Int,
     maxElapsedTime: FiniteDuration
   ): RetryPlan[F] = RetryPlan(maxAttempts, Some(maxElapsedTime))

  /**
   * Creates a RetryPlan with the given maxAttempts and no time limit
   * @param maxAttempts maximum number of attempts
   * @return
   */
  def retry[F[_]](maxAttempts: Int): RetryPlan[F] = RetryPlan(maxAttempts, None)

  /**
   * Creates a RetryPlan with no max attempts and the given maxElapsedTime
   * @param maxElapsedTime maximum elapsed time since start of retry call to run another attempt
   * @return
   */
  def retry[F[_]](maxElapsedTime: FiniteDuration): RetryPlan[F] = RetryPlan(Int.MaxValue, Some(maxElapsedTime))

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
  implicit class RetryFunctionExt[M[_],A,R](
    val self: A => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): A => M[R] =
      { a => retryPlan(self(a)) }
  }

  implicit class RetryFunction2Ext[M[_],A,B,R](
    val self: (A,B) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B) => M[R] =
      { (a,b) => retryPlan(self(a,b)) }
  }

  implicit class RetryFunction3Ext[M[_],A,B,C,R](
    val self: (A,B,C) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C) => M[R] =
      { (a,b,c) => retryPlan(self(a,b,c)) }
  }

  implicit class RetryFunction4Ext[M[_],A,B,C,D,R](
    val self: (A,B,C,D) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C,D) => M[R] =
      { (a,b,c,d) => retryPlan(self(a,b,c,d)) }
  }

  implicit class RetryFunction5Ext[M[_],A,B,C,D,E,R](
    val self: (A,B,C,D,E) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C,D,E) => M[R] =
      { (a,b,c,d,e) => retryPlan(self(a,b,c,d,e)) }
  }

  implicit class RetryFunction6Ext[M[_],A,B,C,D,E,F,R](
    val self: (A,B,C,D,E,F) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C,D,E,F) => M[R] =
      { (a,b,c,d,e,f) => retryPlan(self(a,b,c,d,e,f)) }
  }

  implicit class RetryFunction7Ext[M[_],A,B,C,D,E,F,G,R](
    val self: (A,B,C,D,E,F,G) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C,D,E,F,G) => M[R] =
      { (a,b,c,d,e,f,g) => retryPlan(self(a,b,c,d,e,f,g)) }
  }

  implicit class RetryFunction8Ext[M[_],A,B,C,D,E,F,G,H,R](
    val self: (A,B,C,D,E,F,G,H) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C,D,E,F,G,H) => M[R] =
      { (a,b,c,d,e,f,g,h) => retryPlan(self(a,b,c,d,e,f,g,h)) }
  }

  implicit class RetryFunction9Ext[M[_],A,B,C,D,E,F,G,H,I,R](
    val self: (A,B,C,D,E,F,G,H,I) => M[R]
  ) extends AnyVal {
    def withRetry(
      retryPlan: RetryPlan[M]
    )(implicit
      retryConfig: RetryConfig[M],
      monadErrorM: MonadError[M, Throwable],
      sleepM: Sleep[M],
      clockM: Clock[M]
    ): (A,B,C,D,E,F,G,H,I) => M[R] =
      { (a,b,c,d,e,f,g,h,i) => retryPlan(self(a,b,c,d,e,f,g,h,i)) }
  }
}

