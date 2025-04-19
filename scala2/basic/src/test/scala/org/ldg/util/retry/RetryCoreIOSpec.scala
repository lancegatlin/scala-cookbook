package org.ldg.util.retry

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration.FiniteDuration

class RetryCoreIOSpec extends RetryCoreGenericSpec[IO] with ScalaFutures {
  override protected def testRunSync[A](f: IO[A], timeoutDuration: FiniteDuration): A =
    f.unsafeRunTimed(timeoutDuration).getOrElse(throw new RuntimeException("Timeout"))
}