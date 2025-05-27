package org.ldg.concurrent

import cats.effect.kernel.{Fiber, Poll, Sync}
import cats.effect.{Async, Cont, Ref, Deferred => CatsDeferred}
import org.ldg.OrgLdgUtilAnyExt

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{DurationLong, FiniteDuration}

/**
  * An Async implementation for FuturePlan
  * Note: when using cats syntax extension methods, the ExecutionContext is not bound to specific FutureMap.map/flatMap
  * calls, but rather to the FuturePlanAsync instance's ExecutionContext itself. This subtly changes the default behavior
  * of Future's automatic async boundaries from the implicit ExecutionContext bound at each Future.map/flatMap call.
  *
  * @param executionContext the execution context to run FuturePlan computations that aren't bound by specific
  *                         FutureMap.map/flatMap calls
  * @param config the configuration for FuturePlanAsync
  * @param blockingExecutionContext the execution context to run blocking computations
  */
class FuturePlanAsync(
  config: FuturePlanAsync.Config = FuturePlanAsync.defaultConfig
)(
    implicit
    executionContext: ExecutionContext,
    blockingExecutionContext: BlockingExecutionContext )
    extends Async[FuturePlan] {

  override def pure[A]( x: A ): FuturePlan[A] =
    FuturePlan.pure( x )
  override def map[A, B]( fa: FuturePlan[A] )( f: A => B ): FuturePlan[B] =
    FuturePlan.Map( fa, f, implicitly )
  override def flatMap[A, B]( fa: FuturePlan[A] )( f: A => FuturePlan[B] ): FuturePlan[B] =
    FuturePlan.FlatMap( fa, f, implicitly )
  override def tailRecM[A, B]( a: A )( f: A => FuturePlan[Either[A, B]] ): FuturePlan[B] =
    FuturePlan.TailRecM( a, f )
  override def raiseError[A]( e: Throwable ): FuturePlan[A] =
    FuturePlan.failed( e )
  override def handleErrorWith[A]( fa: FuturePlan[A] )( f: Throwable => FuturePlan[A] ): FuturePlan[A] =
    FuturePlan.HandleErrorWith( fa, f )

  override def suspend[A]( hint: Sync.Type )( thunk: => A ): FuturePlan[A] =
    hint match {
      case Sync.Type.Delay =>
        FuturePlan.delay( thunk )
      case Sync.Type.Blocking =>
        FuturePlan.delay( thunk )( blockingExecutionContext.executionContext )
      case Sync.Type.InterruptibleOnce | Sync.Type.InterruptibleMany =>
        throw new UnsupportedOperationException("Interruptible suspend is not supported in FuturePlanAsync")
    }

  override def executionContext: FuturePlan[ExecutionContext] =
    FuturePlan.pure( executionContext )
  override def evalOn[A]( fa: FuturePlan[A], ec: ExecutionContext ): FuturePlan[A] =
    FuturePlan.evalOn( fa, ec )

  override def start[A]( fa: FuturePlan[A] ): FuturePlan[Fiber[FuturePlan, Throwable, A]] =
    FuturePlan.delay( new FuturePlanFiber[A]( fa, config.futurePlanFiberConfig ).tap( _.start() ) )

  override def forceR[A, B]( fa: FuturePlan[A] )( fb: FuturePlan[B] ): FuturePlan[B] =
    FuturePlan.ForceR( fa, fb )
  override def canceled: FuturePlan[Unit] =
    FuturePlan.Canceled
  override def onCancel[A]( fa: FuturePlan[A], fin: FuturePlan[Unit] ): FuturePlan[A] =
    FuturePlan.OnCancel( fa, fin )
  override def uncancelable[A]( body: Poll[FuturePlan] => FuturePlan[A] ): FuturePlan[A] =
    FuturePlan.Uncancelable( body )

  override def monotonic: FuturePlan[FiniteDuration] =
    FuturePlan.pure( System.nanoTime().nanos )
  override def realTime: FuturePlan[FiniteDuration] =
    FuturePlan.pure( System.currentTimeMillis().millis )

  override protected def sleep( time: FiniteDuration ): FuturePlan[Unit] =
    FuturePlan.Sleep( time )

  override def cede: FuturePlan[Unit] =
    FuturePlan.Cede

  override def ref[A]( a: A ): FuturePlan[Ref[FuturePlan, A]] =
    Ref.in( a )( this, this )
  override def deferred[A]: FuturePlan[CatsDeferred[FuturePlan, A]] =
    CatsDeferred.in( this, this )
  override def cont[K, R]( body: Cont[FuturePlan, K, R] ): FuturePlan[R] =
    Async.defaultCont( body )( this )
}

object FuturePlanAsync {
  case class Config(
    futurePlanFiberConfig: FuturePlanFiber.Config
  )
  val defaultConfig: Config = Config(
    futurePlanFiberConfig = FuturePlanFiber.defaultConfig
  )
}
