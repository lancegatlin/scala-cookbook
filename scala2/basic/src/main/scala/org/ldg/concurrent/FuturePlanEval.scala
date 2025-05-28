package org.ldg.concurrent

import cats.implicits.{catsSyntaxApplyOps, toFlatMapOps}
import org.ldg.concurrent.FuturePlan._
import org.ldg.util.AnyTapExt.OrgLdgUtilAnyTapExt

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.{Failure, Success}

trait FuturePlanEval {
  def apply[A]( plan: FuturePlan[A] )( implicit executionContext: ExecutionContext ): FuturePlanCancelableEval[A]
}

object FuturePlanEval {
  implicit val default: FuturePlanEval = new FuturePlanEval {
    override def apply[A]( plan: FuturePlan[A] )( implicit executionContext: ExecutionContext ): FuturePlanCancelableEval[A] =
      eval( plan )
  }

  private case class EvalState(
    ignoreCancelMaskDepth: Int,
    cancelFlag: AtomicBoolean
  )

  /*
    there are three phases ("times") of FuturePlan evaluation to consider when reviewing the eval method:

      1. "eval time": the time when a reified FuturePlan chain/list is constructed and walked in reverse order until a
      terminal step is reached which has no previous step (i.e. Pure, Delayed, Deferred). This step then is evaluated
      into a base Future which checks the cancel flag before evaluating the step.

      2. "runtime": the background execution of the base Future and the stacking/execution of Future callbacks onto the
      base Future. The base Future and each Future callback pass EvalState to the next step. Each Future callback checks
      the cancel flag before evaluating the step.

      3. "recursive eval runtime": the time when a new FuturePlan is created at runtime (e.g. FlatMap, TailRecM, etc)
      and is then recursively evaluated by calling evalStep. This is similar to eval time, but the state passed to the
      first step of the new FuturePlan chain/list is the last runtime state of the previous chain/list, not the initial
      state of the outer chain/list.
   */
  def eval[T](plan: FuturePlan[T] )(implicit executionContext: ExecutionContext ): FuturePlanCancelableEval[T] = {
    val cancelFlag = new AtomicBoolean(false)
    val state = EvalState(0, cancelFlag)

    val future = evalStep( state, plan ).map( _._2 ).transform {
      case Success( Right( value ) ) => Success( value )
      case Success( Left( ex ) )     => Failure( ex )
      case Failure( ex )             => Failure( ex )
    }

    FuturePlanCancelableEval(
      future,
      { () =>
        cancelFlag.set(true)
        future.transformWith { _ => Future.successful( () ) } // ignore exceptions here, only completion matters
      }
    )
  }

  private def checkCancelFlag(state: EvalState)(implicit executionContext: ExecutionContext) : Future[Unit] =
   // use Future here to ensure cancellation check happens at runtime just before evaluating each step of the FuturePlan
    Future.unit.flatMap { _ =>
      if (state.ignoreCancelMaskDepth == 0 && state.cancelFlag.get) {
        // note: failed Future here will short-circuit all further eval/Future.transforms (e.g. map/flatMap) except for
        // onCancel finalizers which use Future.transformWith (also evalTailRecM which uses onComplete but it ends its
        // loop early by propagating Future.failed)
        Future.failed(new CanceledEvalException( "FuturePlanEval received cancellation signal"))
      } else {
        Future.unit
      }
    }

  private def evalStep[A](
    // when recursing on previous step (e.g. Map, OnCancel, etc), state0 is the initial state of the chain of linked
    // FuturePlans (eval time)
    // however, when recursing during runtime (e.g. FlatMap, TailRecM, etc), state0 is the runtime state at the time the
    // new FuturePlan chain was created
    // also, for Uncancelable or Polling, state0 is used to modify state0.ignoreCancelMaskDepth for their previous steps
    // and later restored during runtime after completion of previous steps
    state0: EvalState,
    fa: FuturePlan[A]
  )(implicit
    executionContext: ExecutionContext
  ): Future[( EvalState, Either[Throwable, A] )] = {
    // evaluate terminal steps which have no previous step (i.e. Pure, Delayed, Deferred) with cancel flag check
    // before evaluating the step
    def evalTerminalStep[T](
      createStepFuture: => Future[( EvalState, Either[Throwable, T] )]
    ): Future[( EvalState, Either[Throwable, T] )] =
      checkCancelFlag(state0) *>
      createStepFuture

    // when recursing on previous step, check cancel flag after building the Future for the previous step to ensure
    // we use the runtime state of the previous step (and not state0)
    def evalPrevStep[T](state: EvalState, fa: FuturePlan[T]): Future[( EvalState, Either[Throwable, T] )] =
      evalStep(state, fa)
        .flatTap { case ( runtimeState, _ ) =>
          checkCancelFlag(runtimeState)
        }

    fa match {
      case Pure( value ) =>
        evalTerminalStep {
          Future.successful( ( state0, value ) )
        }

      case Delayed( thunk, boundExecutionContext ) =>
        evalTerminalStep {
          Future( thunk() )( boundExecutionContext )
            .transform {
              case Success( a )  => Success( ( state0, Right( a ) ) )
              case Failure( ex ) => Success( ( state0, Left( ex ) ) )
            }
        }

      case Deferred( runFuture ) =>
        evalTerminalStep {
          runFuture()
            .transform {
              case Success( a )  => Success( ( state0, Right( a ) ) )
              case Failure( ex ) => Success( ( state0, Left( ex ) ) )
            }
        }

      case Map( prevStep, f, boundExecutionContext ) =>
        evalPrevStep( state0, prevStep )
          .map {
            case ( runtimeState, result ) =>
              ( runtimeState, result.map( f ) )
          }( boundExecutionContext )

      // steps with runtime recursion

      case FlatMap( prevStep, f, boundExecutionContext ) =>
        evalPrevStep( state0, prevStep )
          .flatMap {
            case ( runtimeState, Right( value ) ) =>
              evalStep( runtimeState, f( value ) )
            case ( runtimeState, Left( ex ) ) =>
              Future.successful( ( runtimeState, Left( ex ) ) )
          }( boundExecutionContext )

      case HandleErrorWith( prevStep, f ) =>
        evalPrevStep( state0, prevStep )
          .flatMap {
            case ( runtimeState, Left( ex ) )=>
              evalStep( runtimeState, f( ex ) )
            case other =>
              Future.successful(other)
          }

      case ForceR( fa, fb ) =>
        evalPrevStep( state0, fa )
          // short-circuit fb only if fa canceled (even ignore fa errors)
          // Future will short circuit flatMap if fa is completed with a CancelEvalException failure
          .flatMap { _ =>
            // throw away fa success or failure, only fb matters
            // maybe-do: not sure if should report unhandled fa errors? seems like design of forceR is to ignore them?
            evalStep( state0, fb )
          }

      case tailRecM@TailRecM( _, _ ) =>
        evalTerminalStep {
          evalTailRecM( state0,tailRecM )
        }

      // cancellation
      case Uncancelable( body ) =>
        evalPrevStep(state0.copy(ignoreCancelMaskDepth = state0.ignoreCancelMaskDepth + 1), body( poll ))
          .map { case ( runtimeState, b ) =>
            ( runtimeState.copy( ignoreCancelMaskDepth = runtimeState.ignoreCancelMaskDepth - 1 ), b )
          }

      case OnCancel( prevStep, fin ) =>
        // maybe-do: accumulate finalizers in a runtime list and on cancel run them that way
        evalPrevStep( state0, prevStep )
          // note: can't AndThen here since we want to confirm all finalizers ran by propagation of CanceledEvalException
          // all the way to final future (bottom of evalLoop below)
          .transformWith {
            case Failure(ex:CanceledEvalException) =>
              evalFinalizer( state0, ex, fin )
            case other =>
              Future.fromTry(other)
          }

      case Canceled =>
        // note: no need to check cancelFlag here since we are canceling the FuturePlan eval
        // note: ok for this to run immediately since it will be running in caller's Future
        if (state0.ignoreCancelMaskDepth == 0) {
          Future.failed(new CanceledEvalException( "FuturePlanEval evaluated FuturePlan.canceled" ))
        } else {
          Future.successful( ( state0, Right( () ) ) )
        }

      case Polling( prevStep ) =>
        // reset ignoreCancelMaskDepth to 0 for polling (to allow canceling)
        // note: evalLoop will take care of first checking cancelFlag before eval of prevStep
        evalPrevStep( state0.copy( ignoreCancelMaskDepth = 0 ), prevStep )
          .map {
            case ( runtimeState, result ) =>
              // restore ignoreCancelMaskDepth after polling
              ( runtimeState.copy( ignoreCancelMaskDepth = state0.ignoreCancelMaskDepth ), result )
          }

      case sleep@Sleep( _ ) =>
        evalTerminalStep {
          evalSleep(state0, sleep)
        }

      case Cede =>
        // yield execution by scheduling a callback which take a few micros to complete
        evalTerminalStep {
          Future( ( state0, Right( () ) ) )
        }

    }
  }

  private  def evalTailRecM[Y, Z](
    state: EvalState,
    tailRecM: TailRecM[Y, Z]
  )(implicit
    executionContext: ExecutionContext
  ): Future[( EvalState, Either[Throwable, Z] )] = {
    val promise = Promise[( EvalState, Either[Throwable, Z] )]()
    // stack-safe loop that schedules next loop iteration in the execution context
    def backgroundLoop(state: EvalState, y: Y ): Unit =
      // note: not checking cancelFlag here since each evalStep call will check it
      evalStep( state, tailRecM.f( y ) )
        .onComplete {
          case Success( ( runtimeState, Right( Left( y ) ) ) ) =>
            backgroundLoop( runtimeState, y )
          case Success( ( runtimeState, Right( Right( z ) ) ) ) =>
            promise.complete( Success( ( runtimeState, Right( z ) ) ) )
          case Success( ( runtimeState, Left( ex ) ) ) =>
            promise.complete( Success( ( runtimeState, Left( ex ) ) ) )
          case Failure( ex ) =>
            promise.complete( Failure( ex ) )
        }

    backgroundLoop( state, tailRecM.a )
    promise.future
  }

  private def evalFinalizer[A](
     state: EvalState,
     ex: CanceledEvalException,
     fin: FuturePlan[Unit]
   )(implicit
     executionContext: ExecutionContext
   ): Future[( EvalState, Either[Throwable, A] )] =
    // evaluation of finalizers can't be cancelled
    evalStep( state.copy( ignoreCancelMaskDepth = 1 ), fin )
      .andThen {
        case Success( ( _, Left( ex ) ) ) =>
          executionContext.reportFailure(
            new RuntimeException(
              "FuturePlanEval unexpected exception during finalizer",
              ex
            ) )
      }
      // errors in finalizers are reported & discarded
      .transformWith { _ => Future.failed(ex) }

  private def evalSleep(
    state: EvalState,
    sleep: Sleep
  )(implicit
    executionContext: ExecutionContext
  ): Future[( EvalState, Either[Throwable, Unit] )] = {
    // maybe-do: sleep could also check cancel flag at some interval?
    CompletableFuture
      .supplyAsync[Unit](
        () => (),
        CompletableFuture.delayedExecutor(
          sleep.duration.toNanos,
          TimeUnit.NANOSECONDS,
          executionContext.execute
        )
      )
      .asScala
      .tap( _.onComplete {
        case Failure( ex ) =>
          executionContext.reportFailure(
            new RuntimeException( "FuturePlanEval sleep unexpected exception", ex )
          )
        case _ =>
          // do nothing
      }
    )
    .map( _ => ( state, Right( () ) ) )
  }
}
