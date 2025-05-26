package org.ldg.concurrent

import cats.effect.kernel.Poll
import FuturePlan._

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

case class CancellableEval[A](
    future: Future[A],
    cancelEval: () => Unit
)

trait FuturePlanEval {
  def apply[A]( plan: FuturePlan[A] )( implicit executionContext: ExecutionContext ): CancellableEval[A]
}

object FuturePlanEval {
  implicit val default: FuturePlanEval = new FuturePlanEval {
    override def apply[A]( plan: FuturePlan[A] )( implicit executionContext: ExecutionContext ): CancellableEval[A] =
      defaultEval( plan )
  }

  private val poll = new Poll[FuturePlan] {
    override def apply[T]( fa: FuturePlan[T] ): FuturePlan[T] = Polling( fa )
  }

  case class EvalState(
      ignoreCancelMaskDepth: Int = 0,
      isCancelled: Boolean = false
  )

  private def canceledEvalResult( current: EvalState ): Future[( EvalState, Left[CanceledEvalException, Nothing] )] =
    Future.successful( ( current.copy( isCancelled = true ), Left( new CanceledEvalException( "FuturePlan eval canceled" ) ) ) )

  // scalastyle:off cyclomatic.complexity method.length
  def defaultEval[T]( plan: FuturePlan[T] )( implicit executionContext: ExecutionContext ): CancellableEval[T] = {
    val cancelFlag = new AtomicBoolean( false )

    def evalLoop[A](currentState: EvalState, fa: FuturePlan[A] ): Future[( EvalState, Either[Throwable, A] )] = {
      if (currentState.ignoreCancelMaskDepth > 0 || !cancelFlag.get) {
        fa match {
          case Pure( value ) =>
            Future.successful( ( currentState, value ) )

          case Map( prevStep, f, boundExecutionContext ) =>
            evalLoop( currentState, prevStep )
              .map {
                case ( newState, result ) =>
                  ( newState, result.map( f ) )
              }( boundExecutionContext )

          case FlatMap( prevStep, f, boundExecutionContext ) =>
            evalLoop( currentState, prevStep )
              .flatMap {
                case ( newState, Right( value ) ) =>
                  evalLoop( newState, f( value ) )
                case ( newState, Left( ex ) ) => Future.successful( ( newState, Left( ex ) ) )
              }( boundExecutionContext )

          case HandleErrorWith( prevStep, f ) =>
            evalLoop( currentState, prevStep )
              .transformWith {
                case Success( ( newState, Left( ex ) ) ) =>
                  evalLoop( newState, f( ex ) )
                case success @ Success( _ ) =>
                  Future.fromTry( success )
                case Failure( ex ) => Future.failed( ex )
              }

          case tailRecM@TailRecM( _, _ ) =>
            def evalTailRecM[Y, Z](tailRecM: TailRecM[Y, Z]): Future[( EvalState, Either[Throwable, Z] )] = {
              val promise = Promise[( EvalState, Either[Throwable, Z] )]()
              // stack-safe loop that schedules next loop iteration in the execution context
              def backgroundLoop( current: EvalState, y: Y ): Unit =
                evalLoop( current, tailRecM.f( y ) )
                  .onComplete {
                    case Success( ( state, Right( Left( y ) ) ) ) =>
                      backgroundLoop( state, y )
                    case Success( ( state, Right( Right( z ) ) ) ) =>
                      promise.complete( Success( ( state, Right( z ) ) ) )
                    case Success( ( state, Left( ex ) ) ) =>
                      promise.complete( Success( ( state, Left( ex ) ) ) )
                    case Failure( ex ) =>
                      promise.complete( Failure( ex ) )
                  }

              backgroundLoop( currentState, tailRecM.a )
              promise.future
            }

            evalTailRecM( tailRecM )

          case Delayed( thunk, boundExecutionContext ) =>
            Future( thunk() )( boundExecutionContext ).transform {
              case Success( a )  => Success( ( currentState, Right( a ) ) )
              case Failure( ex ) => Success( ( currentState, Left( ex ) ) )
            }

          case Deferred( runFuture ) =>
            runFuture().transform {
              case Success( a )  => Success( ( currentState, Right( a ) ) )
              case Failure( ex ) => Success( ( currentState, Left( ex ) ) )
            }

          case Uncancelable( body ) =>
            evalLoop(
              currentState.copy(
                ignoreCancelMaskDepth = currentState.ignoreCancelMaskDepth + 1
              ),
              body( poll )
            ).map {
              case ( newState, b ) =>
                ( newState.copy( ignoreCancelMaskDepth = newState.ignoreCancelMaskDepth - 1 ), b )
            }

          case OnCancel( prevStep, fin ) =>
            evalLoop( currentState, prevStep )
              .flatMap {
                case tuple @ ( newState, prevStepEvalResult ) =>
                  if (newState.isCancelled) {
                    // evaluation of finalizers can't be cancelled
                    evalLoop( EvalState( ignoreCancelMaskDepth = 1 ), fin )
                      .andThen {
                        case Success( ( _, Left( ex ) ) ) =>
                          executionContext.reportFailure(
                            new RuntimeException(
                              "FuturePlan.eval unexpected exception during finalizer",
                              ex
                            ) )
                      }
                      // errors in finalizers are suppressed/discarded
                      .map { case ( newState, _ ) => ( newState, prevStepEvalResult ) }
                  } else {
                    Future.successful( tuple )
                  }
              }

          case Canceled() =>
            if (currentState.ignoreCancelMaskDepth == 0) {
              canceledEvalResult( currentState )
            } else {
              Future.successful( ( currentState, Right( () ) ) )
            }

          case Polling( prevStep ) =>
            if (cancelFlag.get) {
              canceledEvalResult( currentState )
            } else {
              // reset ignoreCancelMaskDepth to 0 for polling (to allow canceling)
              evalLoop( currentState.copy( ignoreCancelMaskDepth = 0 ), prevStep )
                .map( {
                  case ( newState, result ) =>
                    // restore ignoreCancelMaskDepth after polling
                    ( newState.copy( ignoreCancelMaskDepth = currentState.ignoreCancelMaskDepth ), result )
                } )
            }

          case ForceR( fa, fb ) =>
            evalLoop( currentState, fa )
              .flatMap {
                case ( newState, _ ) =>
                  // short-circuit fb only if fa cancelled (even ignore fa errors)
                  // maybe-do: not sure if should report unhandled fa errors? seems like design of forceR is to ignore them?
                  if (!newState.isCancelled) {
                    evalLoop( currentState, fb )
                  } else {
                    canceledEvalResult( currentState )
                  }
              }
        }
      } else {
        canceledEvalResult( currentState )
      }
    }
    val future = evalLoop( EvalState(), plan ).map( _._2 ).transform {
      case Success( Right( value ) ) => Success( value )
      case Success( Left( ex ) )     => Failure( ex )
      case Failure( ex )             => Failure( ex )
    }

    CancellableEval( future, () => cancelFlag.set( true ) )
  }
  // scalastyle:on cyclomatic.complexity

}
