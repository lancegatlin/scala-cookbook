package org.ldg.retry

import cats.{Applicative, Monad}
import cats.effect.Clock
import org.ldg.effect.Sleep

import scala.concurrent.duration.FiniteDuration

/**
  * A composite typeclass for retrying operations in a monadic context
  * @tparam F the monadic context (e.g. Future, IO, cats.Id, etc)
  */
trait Retryable[F[_]] extends Monad[F] with Sleep[F] with Clock[F] {
  def retryConfig: RetryConfig[F]
}

object Retryable {
  implicit def default[F[_]](
      implicit
      M: Monad[F],
      S: Sleep[F],
      C: Clock[F],
      retryConfigF: RetryConfig[F] ): Retryable[F] = new Retryable[F] {
    override def retryConfig: RetryConfig[F] = retryConfigF
    override def pure[A]( x: A ): F[A] = M.pure( x )
    override def flatMap[A, B]( fa: F[A] )( f: A => F[B] ): F[B] = M.flatMap( fa )( f )
    override def tailRecM[A, B]( a: A )( f: A => F[Either[A, B]] ): F[B] = M.tailRecM( a )( f )
    override def sleep( duration: FiniteDuration ): F[Unit] = S.sleep( duration )
    override def monotonic: F[FiniteDuration] = C.monotonic
    override def realTime: F[FiniteDuration] = C.realTime
    override def applicative: Applicative[F] = M
  }
}
