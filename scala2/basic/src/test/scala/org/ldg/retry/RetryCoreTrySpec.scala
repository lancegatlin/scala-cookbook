package org.ldg.retry

import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

class RetryCoreTrySpec extends RetryCoreGenericSpec[Try] with ScalaFutures {
  override protected def testRunSync[A]( f: Try[A], timeout: FiniteDuration ): A = f.get
}
