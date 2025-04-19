package org.ldg.effect

import cats.Id
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Success, Try}

class SleepSpec extends AnyFlatSpec with Matchers {
  implicit val executionContext: ExecutionContext = ExecutionContext.global

  "sleepForId" should "block the current thread for the specified duration" in {
    val start = System.nanoTime()
    Sleep[Id].sleep(10.millis)
    val elapsed = (System.nanoTime() - start).nanos
    elapsed should be >= 10.millis
  }

  "sleepForTry" should "return Success after sleeping for the specified duration" in {
    val start = System.nanoTime()
    val result = Sleep[Try].sleep(10.millis)
    val elapsed = (System.nanoTime() - start).nanos
    result shouldBe a[Success[_]]
    elapsed should be >= 10.millis
  }

  "sleepForFuture" should "complete after the specified duration" in {
    val start = System.nanoTime()
    Await.result(Sleep[Future].sleep(10.millis), 20.millis)
    val elapsed = (System.nanoTime() - start).nanos
    elapsed should be >= 10.millis
  }

  "sleepForTemporal" should "delegate to Temporal's sleep method" in {
    import cats.effect.IO
    val start = System.nanoTime()
    Sleep[IO].sleep(10.millis).unsafeRunSync()
    val elapsed = (System.nanoTime() - start).nanos
    elapsed should be >= 10.millis
  }
}