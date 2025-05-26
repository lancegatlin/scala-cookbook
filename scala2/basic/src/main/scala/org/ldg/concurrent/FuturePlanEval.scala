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
    def evalLoop[A]( current: EvalState, fa: FuturePlan[A] ): Future[( EvalState, Either[Throwable, A] )] = {
      if (current.ignoreCancelMaskDepth > 0 || !cancelFlag.get) {
        fa match {
          case Pure( value ) =>
            Future.successful( ( current, value ) )

          case Map( prevStep, f, boundExecutionContext ) =>
            evalLoop( current, prevStep )
              .map {
                case ( state, result ) =>
                  ( state, result.map( f ) )
              }( boundExecutionContext )

          case FlatMap( prevStep, f, boundExecutionContext ) =>
            evalLoop( current, prevStep )
              .flatMap {
                case ( state, Right( value ) ) =>
                  evalLoop( state, f( value ) )
                case ( state, Left( ex ) ) => Future.successful( ( state, Left( ex ) ) )
              }( boundExecutionContext )

          case HandleErrorWith( prevStep, f ) =>
            evalLoop( current, prevStep )
              .transformWith {
                case Success( ( state, Left( ex ) ) ) =>
                  evalLoop( state, f( ex ) )
                case success @ Success( _ ) =>
                  Future.fromTry( success )
                case Failure( ex ) => Future.failed( ex )
              }

          case TailRecM( a, f ) =>
            def evalTailRecM[Y, Z](
                tailRecM: TailRecM[Y, Z]
            ): Future[( EvalState, Either[Throwable, Z] )] = {
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

              backgroundLoop( current, tailRecM.a )
              promise.future
            }

            evalTailRecM( TailRecM( a, f ) )

          case Delayed( thunk, boundExecutionContext ) =>
            Future( thunk() )( boundExecutionContext ).transform {
              case Success( a )  => Success( ( current, Right( a ) ) )
              case Failure( ex ) => Success( ( current, Left( ex ) ) )
            }

          case Deferred( runFuture ) =>
            runFuture().transform {
              case Success( a )  => Success( ( current, Right( a ) ) )
              case Failure( ex ) => Success( ( current, Left( ex ) ) )
            }

          case Uncancelable( body ) =>
            evalLoop(
              current.copy(
                ignoreCancelMaskDepth = current.ignoreCancelMaskDepth + 1
              ),
              body( poll )
            ).map {
              case ( state, b ) =>
                ( state.copy( ignoreCancelMaskDepth = state.ignoreCancelMaskDepth - 1 ), b )
            }

          case OnCancel( prevStep, fin ) =>
            evalLoop( current, prevStep )
              .flatMap {
                case tuple @ ( state, result ) =>
                  if (state.isCancelled) {
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
                      // errors in finalizers are suppressed
                      .map { case ( state, _ ) => ( state, result ) }
                  } else {
                    Future.successful( tuple )
                  }
              }

          case Canceled() =>
            if (current.ignoreCancelMaskDepth == 0) {
              canceledEvalResult( current )
            } else {
              Future.successful( ( current, Right( () ) ) )
            }

          case Polling( prevStep ) =>
            if (cancelFlag.get) {
              canceledEvalResult( current )
            } else {
              evalLoop( current.copy( ignoreCancelMaskDepth = 0 ), prevStep )
                .map( {
                  case ( state, result ) =>
                    ( state.copy( ignoreCancelMaskDepth = current.ignoreCancelMaskDepth ), result )
                } )
            }

          case ForceR( fa, fb ) =>
            evalLoop( current, fa )
              .flatMap {
                case ( state, _ ) =>
                  // short-circuit fb only if fa cancelled (even ignore fa errors)
                  if (!state.isCancelled) {
                    evalLoop( current, fb )
                  } else {
                    canceledEvalResult( current )
                  }
              }
        }
      } else {
        canceledEvalResult( current )
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
