package org.ldg.retry

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileNotFoundException
import scala.concurrent.duration.DurationInt

class RetryCoreSpec extends AnyFlatSpec with Matchers with Inside {
  import org.ldg.retry.RetryCore._

  "defaultGenCorrelationId" should "generate a correlation id" in {
    val correlationId = defaultGenCorrelationId()
    correlationId should not be empty
    correlationId.length should be > 0
    correlationId.length shouldBe 36
  }

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
    val baseDelay = 250.millis
    val maxJitter = 10.millis
    val maxDelay = 10.seconds
    val fixture = retryDelayWithExponentialBackoffAndJitter(
      baseDelay = baseDelay,
      maxJitter = maxJitter,
      maxDelay = maxDelay
    )(_)
    an[IllegalArgumentException] should be thrownBy fixture(0)
    fixture(1) should (be >= baseDelay and be <= baseDelay + maxJitter)
    fixture(2) should (be >= (baseDelay * 2) and be <= (baseDelay * 2) + maxJitter)
    fixture(3) should (be >= (baseDelay * 4) and be <= (baseDelay * 4) + maxJitter)
    fixture(4) should (be >= (baseDelay * 8) and be <= (baseDelay * 8) + maxJitter)
    fixture(5) should (be >= (baseDelay * 16) and be <= (baseDelay * 16) + maxJitter)
    fixture(6) should (be >= (baseDelay * 32) and be <= (baseDelay * 32) + maxJitter)
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
}