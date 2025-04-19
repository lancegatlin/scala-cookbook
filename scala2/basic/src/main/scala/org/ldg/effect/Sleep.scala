package org.ldg.effect

import cats.Id
import cats.effect.Temporal

import java.util.concurrent.{CompletableFuture, Executor, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.Try

/**
 * A type-class for "sleeping" for a specified duration in a monadic context.
 *
 * Note: use this as an alternative to cats.effect.Temporal since Temporal requires cats.effect.Concurrent which cannot
 * be implemented for cats.Id, Try or Future (requires cats.effect.Sync and other missing capabilities)
 */
trait Sleep[F[_]] {
  def sleep(duration: FiniteDuration): F[Unit]
}

object Sleep {
  implicit val sleepForId: Sleep[Id] = new Sleep[Id] {
    override def sleep(duration: FiniteDuration): Id[Unit] =
      Thread.sleep(duration.toMillis)
  }

  implicit val sleepForTry: Sleep[Try] = new Sleep[Try] {
    override def sleep(duration: FiniteDuration): Try[Unit] = {
      Thread.sleep(duration.toMillis)
      scala.util.Success(())
    }
  }

  implicit def sleepForFuture(implicit
    executionContext: ExecutionContext
  ): Sleep[Future] = new Sleep[Future] {
    private val delegateExecutor = new Executor {
      override def execute(command: Runnable): Unit =
        executionContext.execute(command)
    }
    override def sleep(duration: FiniteDuration): Future[Unit] = {
      CompletableFuture.supplyAsync[Unit](
        () => (),
        CompletableFuture.delayedExecutor(
          duration.toNanos,
          TimeUnit.NANOSECONDS,
          delegateExecutor
        )
      ).asScala
    }
  }

  implicit def sleepForTemporal[F[_]:Temporal]: Sleep[F] = new Sleep[F] {
    override def sleep(duration: FiniteDuration): F[Unit] =
      Temporal[F].sleep(duration)
  }

  def apply[F[_]](implicit S: Sleep[F]): Sleep[F] = S
}

