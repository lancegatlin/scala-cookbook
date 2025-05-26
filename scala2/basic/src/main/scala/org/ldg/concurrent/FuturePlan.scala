package org.ldg.concurrent

import cats.effect.Async
import cats.effect.kernel.Poll

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * A FuturePlan is an ADT that represents a computation that results in a Future of a value.
  *
  * FuturePlan is a glue class for use with tagless implementations that allow only effect types that implement the
  * Async typeclass to be implemented (such as Doobie) that may still have downstream code that requires a Future.
  *
  * Unlike Future, a FuturePlan is pure, has referential transparency and is not eager, meaning that it can be
  * constructed and manipulated without executing the computation immediately (or any memoization). This means that
  * while Async[Future] can't be safely implemented, Async[FuturePlan] can.
  *
  * FuturePlan evaluation can be cancelled at runtime, but important to note that any Futures it defers or creates
  * themselves cannot be canceled. Though after Futures complete, the FuturePlan evaluation will cancel normally.
  *
  * To cancel FuturePlan evaluation:
  *   1. use the `Async[FuturePlan].canceled` method
  *   2. use the `CancellableEval.cancelEval` function
  *   3. start a Fiber and call the fiber's `cancel` method
  *
  * @tparam A the type of the result of the computation
  */
sealed trait FuturePlan[A] {
  def run()( implicit executionContext: ExecutionContext, eval: FuturePlanEval ): Future[A] =
    eval( this ).future
  def eval()( implicit executionContext: ExecutionContext, eval: FuturePlanEval ): CancellableEval[A] =
    eval( this )

  // note: the `map` and `flatMap` methods here accept ExecutionContext to preserve Future's expected async boundaries
  def map[B]( f: A => B )( implicit executionContext: ExecutionContext ): FuturePlan[B] =
    FuturePlan.Map( this, f, executionContext )
  def flatMap[B]( f: A => FuturePlan[B] )( implicit executionContext: ExecutionContext ): FuturePlan[B] =
    FuturePlan.FlatMap( this, f, executionContext )
}

object FuturePlan {
  // todo: optimize Map on Pure/Success and FlatMap on Pure/Success to avoid unnecessary wrapping by eager evaluation

  final case class Pure[A]( value: Either[Throwable, A] ) extends FuturePlan[A]

  final case class Map[A, B](
      prevStep: FuturePlan[A],
      f: A => B,
      boundExecutionContext: ExecutionContext
  ) extends FuturePlan[B]

  final case class FlatMap[A, B](
      prevStep: FuturePlan[A],
      f: A => FuturePlan[B],
      boundExecutionContext: ExecutionContext
  ) extends FuturePlan[B]

  final case class Delayed[A]( thunk: () => A, boundExecutionContext: ExecutionContext ) extends FuturePlan[A]
  final case class Deferred[A]( runFuture: () => Future[A] ) extends FuturePlan[A]
  final case class Uncancelable[A]( body: Poll[FuturePlan] => FuturePlan[A] ) extends FuturePlan[A]
  final case class OnCancel[A]( prevStep: FuturePlan[A], fin: FuturePlan[Unit] ) extends FuturePlan[A]
  final case class Canceled() extends FuturePlan[Unit]
  final case class Polling[A]( prevStep: FuturePlan[A] ) extends FuturePlan[A]
  final case class ForceR[A, B]( fa: FuturePlan[A], fb: FuturePlan[B] ) extends FuturePlan[B]
  final case class HandleErrorWith[A]( prevStep: FuturePlan[A], f: Throwable => FuturePlan[A] ) extends FuturePlan[A]
  final case class TailRecM[A, B]( a: A, f: A => FuturePlan[Either[A, B]] ) extends FuturePlan[B]

  def pure[A]( value: A ): FuturePlan[A] =
    Pure( Right( value ) )
  val unit: FuturePlan[Unit] =
    FuturePlan.pure( () )
  def failed[U]( ex: Throwable ): FuturePlan[U] =
    Pure( Left( ex ) )
  def fromTry[A]( value: Try[A] ): FuturePlan[A] =
    value match {
      case Success( v ) => FuturePlan.pure( v )
      case Failure( e ) => FuturePlan.failed( e )
    }
  def fromEither[A]( value: Either[Throwable, A] ): FuturePlan[A] =
    value match {
      case Right( v ) => FuturePlan.pure( v )
      case Left( ex ) => FuturePlan.failed( ex )
    }

  /**
    * Creates a FuturePlan that will run the provided code block when evaluated.
    * Note: it is not possible for the FuturePlan to cancel the Future once it is running. If the FuturePlan is
    * cancelled, then when Future completes, the FuturePlan will complete with a CancellationException (and run
    * finalizers)
    * @param thunk the code block to run when the FuturePlan is evaluated
    * @param executionContext the ExecutionContext to run the code block on
    * @tparam A the type of the result of the code block
    * @return a FuturePlan that will run the code block when evaluated
    */
  def delay[A]( thunk: => A )( implicit executionContext: ExecutionContext ): FuturePlan[A] =
    Delayed( () => thunk, executionContext )

  /**
    * Creates a FuturePlan that will run the provided Future when evaluated.
    * Note1: the Future is not memoized, meaning that it will be executed every time the FuturePlan is run.
    * Note2: it is not possible for the FuturePlan to cancel the Future once it is running. If the FuturePlan is
    * cancelled, then when Future completes, the FuturePlan will complete with a CancellationException (and run
    * finalizers)
    * @param f the Future to create when the FuturePlan is evaluated
    * @tparam A the type of the result of the Future
    * @return a FuturePlan that will run the Future when evaluated
    */
  def defer[A]( f: => Future[A] ): FuturePlan[A] =
    Deferred( () => f )

  /**
    * @return a FuturePlan with all bound ExecutionContexts recursively replaced by the provided one
    */
  def evalOn[A]( fa: FuturePlan[A], executionContext: ExecutionContext ): FuturePlan[A] =
    fa match {
      case fa: Map[_, _] =>
        fa.copy(
          prevStep = evalOn( fa.prevStep, executionContext ),
          boundExecutionContext = executionContext
        )
      case fa: FlatMap[_, _] =>
        fa.copy(
          prevStep = evalOn( fa.prevStep, executionContext ),
          boundExecutionContext = executionContext
        )
      case fa: Delayed[_] =>
        fa.copy(
          boundExecutionContext = executionContext
        )
      case _ => fa
    }

  object Implicits {
    implicit def asyncForFuturePlan(
        implicit
        executionContext: ExecutionContext,
        blockingExecutionContext: BlockingExecutionContext
    ): Async[FuturePlan] =
      new FuturePlanAsync()( executionContext, blockingExecutionContext )
  }
}
