package org.ldg.concurrent

import scala.concurrent.Future

/**
  * A cancellable evaluation of a FuturePlan.
  *
  * @param future the running Future which completes with the result of the FuturePlan evaluation (the value,
  *               an exception or CanceledEvalException if canceled).
  * @param cancelEval a function that cancels the evaluation of the FuturePlan. The returned future never fails and
  *                   completes only after all finalizers complete.
  * @tparam A the type of the result of the FuturePlan
  */
  case class FuturePlanCancelableEval[A](
     future: Future[A],
     cancelEval: () => Future[Unit]
   )
