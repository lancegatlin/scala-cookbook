package org.ldg.util.retry

import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class RetryCoreFutureSpec extends RetryCoreGenericSpec[Future] with ScalaFutures {
  override protected def testRunSync[A](f: Future[A], timeoutDuration: FiniteDuration): A =
    f.futureValue(timeout(timeoutDuration))
}