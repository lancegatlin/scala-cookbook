package org.ldg.effect

import cats.Id
import cats.effect.Sync

import scala.concurrent.{ExecutionContext, Future}

/**
  * A type-class for capturing an effect into a monadic context if supported, or if that the monadic context doesn't
  * support capturing effects, to run the effect immediately. Used as a simpler alternative to Sync, when effect capture
  * is needed but error flow control isn't required, allowing use of the identity monad (Id) for testing or usage in
  * non-monadic code. If the effect function throws an exception, the monadic context is expected to handle it according
  * to its own semantics. For Id, this means the exception will be propagated immediately.
  *
  * Use instead of Sync/Async to allow use of Id, Future and IO monads (Sync[Future] and Sync[Id] do not work)
  *
  * Note: CaptureOrRunEffect can't be used with MonadError since it is not possible to implement MonadError for Id. If
  * MonadError is needed, consider using Sync instead (though this means giving up on using Id) or consider using
  * the attempt method to capture exceptions from the effect in an Either.
  *
  * Example:
  *   class MyTaglessService[F[_]: CaptureOrRunEffect] {
  *      def doSomething: F[Unit] = CaptureOrRunEffect[F].effect( ... ) // capture effect or run it immediately
  *      def doSomethingElse: F[Either[Throwable, Unit]] = CaptureOrRunEffect[F].attempt( ... ) // capture effect or run it and handle exceptions
  *    }
  *    val myServiceUsingId = new MyTaglessService[Id] // requires only standard cats imports
  *     val myServiceUsingFuture = new MyTaglessService[Future] // requires implicit ExecutionContext
  *     val myServiceUsingIO = new MyTaglessService[IO] // requires implicit Sync[IO] instance import
  *
  * @tparam F a monadic context (Future, IO, etc) or the identity monad (Id)
  */
trait CaptureOrRunEffect[F[_]] {
  def effect[A]( a: => A ): F[A]

  def attempt[A]( a: => A ): F[Either[Throwable, A]] =
    effect(
      try Right( a )
      catch {
        case e: Throwable => Left( e )
      } )
}

object CaptureOrRunEffect {
  def apply[F[_]]( implicit e: CaptureOrRunEffect[F] ): CaptureOrRunEffect[F] = e

  implicit def captureOrRunEffectForId: CaptureOrRunEffect[Id] = new CaptureOrRunEffect[Id] {
    override def effect[A]( a: => A ): Id[A] = a
  }

  implicit def captureOrRunEffect( implicit ec: ExecutionContext ): CaptureOrRunEffect[Future] = new CaptureOrRunEffect[Future] {
    override def effect[A]( a: => A ): Future[A] = Future( a )
  }

  implicit def captureOrRunEffectForSync[F[_]]( implicit S: Sync[F] ): CaptureOrRunEffect[F] = new CaptureOrRunEffect[F] {
    override def effect[A]( a: => A ): F[A] = S.delay( a )
  }
}
