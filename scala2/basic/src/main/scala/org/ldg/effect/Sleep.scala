package org.ldg.effect

import cats.Id
import cats.effect.Temporal

import java.util.concurrent.{CompletableFuture, Executor, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.Try

/**
  * A type-class for "sleeping" for a specified duration in a monadic context, which may be blocking or non-blocking,
  * deferred or immediate depending on the context (e.g. cats.Id vs cats.effect.IO).
  *
  * Note: use this as an alternative to cats.effect.Temporal since Temporal requires cats.effect.Concurrent which cannot
  * be implemented for cats.Id, Try or Future (requires cats.effect.Sync and other missing capabilities)
  */
trait Sleep[F[_]] {
  def sleep( duration: FiniteDuration ): F[Unit]
}

object Sleep {
  implicit val sleepForId: Sleep[Id] = { duration =>
    Thread.sleep( duration.toMillis )
  }

  implicit val sleepForTry: Sleep[Try] = { duration =>
    scala.util.Success( Thread.sleep( duration.toMillis ) )
  }

  implicit def sleepForFuture( implicit executionContext: ExecutionContext ): Sleep[Future] = {
    val delegateExecutor: Executor = command => executionContext.execute( command )

    { duration =>
      CompletableFuture
        .supplyAsync[Unit](
          () => (),
          CompletableFuture.delayedExecutor(
            duration.toNanos,
            TimeUnit.NANOSECONDS,
            delegateExecutor
          )
        ).asScala
    }
  }

  implicit def sleepForTemporal[F[_]: Temporal]: Sleep[F] = { duration =>
    Temporal[F].sleep( duration )
  }

  def apply[F[_]]( implicit S: Sleep[F] ): Sleep[F] = S
}
