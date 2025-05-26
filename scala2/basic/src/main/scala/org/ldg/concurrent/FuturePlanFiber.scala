package org.ldg.concurrent

import cats.effect.kernel.{Fiber, Outcome}
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * A Fiber implementation for FuturePlan that allows for cancellation and joining of the FuturePlan evaluation.
  * @param fa the FuturePlan to evaluate
  * @param executionContext the execution context to run the FuturePlan evaluation
  * @param eval the FuturePlanEval instance to evaluate the FuturePlan
  * @tparam A the type of the result of the FuturePlan
  */
class FuturePlanFiber[A](
    fa: FuturePlan[A]
)(
    implicit
    executionContext: ExecutionContext,
    eval: FuturePlanEval )
    extends Fiber[FuturePlan, Throwable, A] {

  private val isJoined = new AtomicBoolean( false )
  private lazy val cancellableEval: FuturePlanCancelableEval[A] =
    eval( fa )
      .tap {
        _.future.onComplete {
          case Failure( ex ) =>
            if (!isJoined.get) {
              executionContext.reportFailure(
                new RuntimeException( "Unexpected exception while evaluating background FuturePlanFiber (which was never joined)", ex )
              )
            }
          case _ => // do nothing
        }
      }

  def start(): FuturePlanCancelableEval[A] = cancellableEval

  override def cancel: FuturePlan[Unit] =
    FuturePlan.defer( cancellableEval.cancelEval() )

  override def join: FuturePlan[Outcome[FuturePlan, Throwable, A]] =
    FuturePlan.defer {
      isJoined.set( true )
      cancellableEval
        .future
        .transform {
          case Success( a ) =>
            Success( Outcome.succeeded( FuturePlan.Pure( Right( a ) ) ) )
          case Failure( _: CancellationException ) =>
            Success( Outcome.canceled )
          case Failure( ex ) =>
            Success( Outcome.errored( ex ) )
        }
    }
}
