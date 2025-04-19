package org.ldg.effect

import cats.effect.kernel.Clock
import cats.{Applicative, Id, Monad}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/**
  * A type-class that composes the capabilities of capturing side-effects and running them in a monadic context with
  * the capabilities of a monad and a clock.
  *
  * Use instead of Sync/Async to allow use of Id, Future and IO monads (Sync[Future] and Sync[Id] do not work)
  *
  * Example:
  *   class MyTaglessService[F[_]: Effect] {
  *      def doSomething: F[Unit] = Effect[F].effect( ... ) // capture effect or run it immediately
  *      def doSomethingElse: F[Either[Throwable, Unit]] = Effect[F].attempt( ... ) // capture effect or run it and handle exceptions
  *    }
  *    val myServiceUsingId = new MyTaglessService[Id] // requires only standard cats imports
  *    val myServiceUsingFuture = new MyTaglessService[Future] // requires implicit ExecutionContext
  *    val myServiceUsingIO = new MyTaglessService[IO] // requires implicit Sync[IO] instance import
  *
  * @tparam F a monadic context (Future, IO, etc) or the identity monad (Id)
  */
trait Effect[F[_]] extends CaptureOrRunEffect[F] with Monad[F] with Clock[F] with Sleep[F]

object Effect {
  class DelegatingEffect[F[_]]()( implicit E: CaptureOrRunEffect[F], M: Monad[F], C: Clock[F], S: Sleep[F] ) extends Effect[F] {
    override def applicative: Applicative[F] = implicitly

    override def effect[A]( a: => A ): F[A] = E.effect( a )

    override def pure[A]( x: A ): F[A] = M.pure( x )
    override def flatMap[A, B]( fa: F[A] )( f: A => F[B] ): F[B] = M.flatMap( fa )( f )
    override def tailRecM[A, B]( a: A )( f: A => F[Either[A, B]] ): F[B] = M.tailRecM( a )( f )

    override def monotonic: F[FiniteDuration] = C.monotonic
    override def realTime: F[FiniteDuration] = C.realTime

    override def sleep(duration: FiniteDuration): F[Unit] = S.sleep( duration )
  }

  def apply[F[_]]( implicit e: Effect[F] ): Effect[F] = e

  implicit def effectForId( implicit E: CaptureOrRunEffect[Id], M: Monad[Id], C: Clock[Id], S: Sleep[Id] ): Effect[Id] =
    new DelegatingEffect[Id]()( E, M, C, S )

  implicit def effectForFuture( implicit E: CaptureOrRunEffect[Future], M: Monad[Future], C: Clock[Future], S: Sleep[Future] ): Effect[Future] =
    new DelegatingEffect[Future]()( E, M, C, S)

  implicit def effectForSync[F[_]]( implicit E: CaptureOrRunEffect[F], S: cats.effect.Sync[F], SL: Sleep[F] ): Effect[F] =
    new DelegatingEffect[F]()( E, S, S, SL )
}
