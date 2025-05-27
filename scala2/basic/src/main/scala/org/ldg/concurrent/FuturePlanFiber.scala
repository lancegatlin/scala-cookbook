package org.ldg.concurrent

import cats.effect.kernel.{Fiber, Outcome}
import org.ldg.util.AnyTapExt.OrgLdgUtilAnyTapExt

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success}

/**
  * A Fiber implementation for FuturePlan that allows for cancellation and joining of the FuturePlan evaluation.
  * @param fa the FuturePlan to evaluate
  * @param config the configuration for FuturePlanFiber (see FuturePlanFiber.Config for details)
  * @param executionContext the execution context to run the FuturePlan evaluation
  * @param eval the FuturePlanEval instance to evaluate the FuturePlan
  * @tparam A the type of the result of the FuturePlan
  */
class FuturePlanFiber[A](
  fa: FuturePlan[A],
  config: FuturePlanFiber.Config = FuturePlanFiber.defaultConfig
)(implicit
    executionContext: ExecutionContext,
    eval: FuturePlanEval
) extends Fiber[FuturePlan, Throwable, A] {

  private val isJoined = new AtomicBoolean( false )
  private lazy val cancelableEval: FuturePlanCancelableEval[A] =
    eval( fa )
      .tapIf(config.maybeFailureJoinTimeout) { (cancelableEval, failureJoinTimeout) =>
        FuturePlan.sleep(failureJoinTimeout).run()
          .andThen { _ =>
            cancelableEval.future.onComplete {
              case Failure( ex ) =>
                if (!isJoined.get) {
                  executionContext.reportFailure(
                    new RuntimeException( "FuturePlanFiber unexpected exception from background FuturePlanEval which was never joined", ex )
                  )
                }
              case _ => // do nothing
            }
          }
      }

  def start(): FuturePlanCancelableEval[A] =
    cancelableEval

  override def cancel: FuturePlan[Unit] =
    FuturePlan.defer( cancelableEval.cancelEval() )

  override def join: FuturePlan[Outcome[FuturePlan, Throwable, A]] =
    FuturePlan.defer {
      isJoined.set( true )
      cancelableEval
        .future
        .transform {
          case Success( a ) =>
            Success( Outcome.succeeded( FuturePlan.Pure( Right( a ) ) ) )
          case Failure( _: CanceledEvalException ) =>
            Success( Outcome.canceled )
          case Failure( ex ) =>
            Success( Outcome.errored( ex ) )
        }
    }
}

object FuturePlanFiber {
  /**
   * @param maybeFailureJoinTimeout if the FuturePlan ends with an exception and is not joined within this duration, then
   *                                the exception is treated as an unhandled exception and forwarded to
   *                                executionContext.reportFailure. Set to None to disable this check.
   */
  case class Config(
    maybeFailureJoinTimeout: Option[FiniteDuration]
  )
  val defaultConfig: Config = Config(
    // note: somewhat arbitrary timeout value
    maybeFailureJoinTimeout = Some(250.millis)
  )
}