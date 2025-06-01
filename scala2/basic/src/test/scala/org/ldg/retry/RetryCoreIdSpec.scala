package org.ldg.retry

import cats.Id
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration.FiniteDuration

class RetryCoreIdSpec extends RetryCoreGenericSpec[Id] with ScalaFutures {
  override protected def testRunSync[A]( f: Id[A], timeout: FiniteDuration ): A = f
}
