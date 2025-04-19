package org.ldg.util.retry.old

import org.ldg.util.retry.old.RetryUtils._
import org.scalatest.Inside
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileNotFoundException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionException, Future}
import scala.util.{Failure, Success}

class RetryUtilsSpec extends AnyFlatSpec with Matchers with ScalaFutures with Inside {

  "defaultShouldRetry" should "return false for unretryable exceptions" in {
    val unretryable = Seq(
      new FileNotFoundException,
      new ReflectiveOperationException,
      new IllegalArgumentException,
      new ClassCastException,
      new TypeNotPresentException("", new RuntimeException()),
      new UnsupportedOperationException,
      new CloneNotSupportedException,
      // fatal exception
      new ThreadDeath
    )
    unretryable.foreach { ex =>
      defaultShouldRetry(ex) shouldBe false
    }
  }

  "retryDelayWithExponentialBackoffAndJitter" should "calculate correct delay for a given retry" in {
    val initialDelay = 250.millis
    val maxJitter = 10.millis
    val maxDelay = 10.seconds
    val fixture = retryDelayWithExponentialBackoffAndJitter(
      initialDelay = initialDelay,
      maxJitter = maxJitter,
      maxDelay = maxDelay
    )(_)
    an[IllegalArgumentException] should be thrownBy fixture(0)
    fixture(1) should (be >= initialDelay and be <= initialDelay + maxJitter)
    fixture(2) should (be >= (initialDelay * 2) and be <= (initialDelay * 2) + maxJitter)
    fixture(3) should (be >= (initialDelay * 4) and be <= (initialDelay * 4) + maxJitter)
    fixture(4) should (be >= (initialDelay * 8) and be <= (initialDelay * 8) + maxJitter)
    fixture(5) should (be >= (initialDelay * 16) and be <= (initialDelay * 16) + maxJitter)
    fixture(6) should (be >= (initialDelay * 32) and be <= (initialDelay * 32) + maxJitter)
    fixture(7) shouldBe maxDelay
    fixture(8) shouldBe maxDelay
  }

  "defaultRetryDelayWithExponentialBackoffAndJitter" should "calculate correct delay for a given retry" in {
    val initialDelay = 250.millis
    val maxJitter = 10.millis
    val maxDelay = 10.seconds
    val fixture = defaultRetryDelayWithExponentialBackoffAndJitter
    an[IllegalArgumentException] should be thrownBy fixture(0)
    fixture(1) should (be >= initialDelay and be <= initialDelay + maxJitter)
    fixture(2) should (be >= (initialDelay * 2) and be <= (initialDelay * 2) + maxJitter)
    fixture(3) should (be >= (initialDelay * 4) and be <= (initialDelay * 4) + maxJitter)
    fixture(4) should (be >= (initialDelay * 8) and be <= (initialDelay * 8) + maxJitter)
    fixture(5) should (be >= (initialDelay * 16) and be <= (initialDelay * 16) + maxJitter)
    fixture(6) should (be >= (initialDelay * 32) and be <= (initialDelay * 32) + maxJitter)
    fixture(7) shouldBe maxDelay
    fixture(8) shouldBe maxDelay
  }

  "recurseUntilUltimateCause" should "recurse until it returns the ultimate cause in a chain of throwables" in {
    inside(recurseUntilUltimateCause(new RuntimeException("1", new RuntimeException("2", new RuntimeException("3"))))) {
      case ex: RuntimeException =>
        ex.getMessage shouldBe "3"
    }
    inside(recurseUntilUltimateCause(new RuntimeException("1"))) {
      case ex: RuntimeException =>
        ex .getMessage shouldBe "1"
    }
  }

  class Fixture(
    val maxAttempts: Int,
    val retryDelay: FiniteDuration
  ) {
    val capturedFailures = List.newBuilder[Throwable]

    val fixture = runRetryState[String, Int](
      onBeforeAttempt = { attemptCount =>
        val newAttemptCount = attemptCount + 1
        capturedAttemptCounts += newAttemptCount
        Future.successful(newAttemptCount)
      },
      onAfterFailure = { (attemptCount, ex) =>
        capturedFailures += ex
        if (attemptCount < maxAttempts) {
          Future.successful(Some((attemptCount, retryDelay)))
        } else {
          Future.successful(None)
        }
      }
    )(
      initialState = 0
    )(_)

    val capturedAttemptCounts = List.newBuilder[Int]
    val effectSucceedsOn3rdAttempt = { attemptCount: Int =>
      if (attemptCount < 3) {
        Future.failed(new RuntimeException(s"failed $attemptCount"))
      } else {
        Future.successful("success")
      }
    }
}


  "runRetryState" should "retry functions that fail when onAfterFailure returns Some" in new Fixture(5, 0.seconds) {
    fixture(effectSucceedsOn3rdAttempt).futureValue shouldBe (3, Success("success"))
    capturedAttemptCounts.result() shouldBe List(1, 2, 3)
    capturedFailures.result().size shouldBe 2
  }

  it should "stop retrying functions that fail when onAfterFailure returns None" in new Fixture(2, 0.seconds) {
    inside(fixture(effectSucceedsOn3rdAttempt).futureValue) {
      case (2, Failure(ex)) =>
        ex.getMessage shouldBe "failed 2"
        ex shouldBe a[RuntimeException]
    }
    capturedAttemptCounts.result() shouldBe List(1, 2)
    capturedFailures.result().size shouldBe 2
  }

  it should "delay retrying function that fail when onAfterFailure returns Some delay" in new Fixture(5, 250.millis) {
    val startTimeMs = System.currentTimeMillis()

    fixture(effectSucceedsOn3rdAttempt).futureValue(timeout(retryDelay * 4)) shouldBe (3, Success("success"))
    capturedAttemptCounts.result() shouldBe List(1, 2, 3)
    capturedFailures.result().size shouldBe 2
    val elapsedMs = System.currentTimeMillis() - startTimeMs
    elapsedMs should be >= retryDelay.toMillis
  }

  it should "stop retrying functions that fail with a fatal exception" in new Fixture(2, 0.seconds) {
    inside(fixture(_ => Future.failed(new ThreadDeath)).futureValue) {
      case (1, Failure(ex)) =>
        ex shouldBe a[ExecutionException]
        ex.getCause shouldBe a[ThreadDeath]
    }
    capturedAttemptCounts.result() shouldBe List(1)
    capturedFailures.result().size shouldBe 0
  }
}