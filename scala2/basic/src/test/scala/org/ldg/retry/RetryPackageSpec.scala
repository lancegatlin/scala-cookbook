package org.ldg.retry

import java.util.concurrent.atomic.AtomicInteger
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class RetryPackageSpec extends AnyFlatSpec with Matchers {
  implicit val defaultRetryConfig: RetryConfig[IO] = RetryConfig.default[IO].copy(
    calcRetryDelay = { (_, _) => 0.millis }
  )

  "retry with maxAttempts" should "retry the specified number of times" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val result = retryPlan.run {
      IO {
        if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
        "Success"
      }
    }.unsafeRunSync()

    result shouldBe "Success"
    attempts.get() shouldBe 3
  }

  "retry with maxElapsedTime" should "stop retrying after the specified time" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](100.millis)
    val exception = new RuntimeException("Failure")
    val alwaysFalse = false

    val result = retryPlan.run {
      IO {
        attempts.incrementAndGet()
        if(alwaysFalse) {
          0
        } else {
          throw exception
        }
      }
    }.attempt.unsafeRunTimed(1.second)

    result shouldBe Some(Left(exception))
    attempts.get() should be > 3
  }

  "retry with maxAttempts and maxElapsedTime" should "respect both constraints" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3, 1.second)

    val result = retryPlan.run {
      IO {
        if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
        "Success"
      }
    }.unsafeRunSync()

    result shouldBe "Success"
    attempts.get() shouldBe 3
  }

  "RetryFunctionExt" should "wrap a function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: String => IO[String] = input => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"Hello, $input"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("World").unsafeRunSync()

    result shouldBe "Hello, World"
    attempts.get() shouldBe 3
  }

  "RetryFunction2Ext" should "wrap a two-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int) => IO[String] = (input, number) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42).unsafeRunSync()

    result shouldBe "Hello, 42"
    attempts.get() shouldBe 3
  }

  "RetryFunction3Ext" should "wrap a three-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean) => IO[String] = (input, number, flag) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true).unsafeRunSync()

    result shouldBe "Hello, 42, true"
    attempts.get() shouldBe 3
  }

  "RetryFunction4Ext" should "wrap a four-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean, Double) => IO[String] = (input, number, flag, value) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag, $value"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true, 3.14).unsafeRunSync()

    result shouldBe "Hello, 42, true, 3.14"
    attempts.get() shouldBe 3
  }

  "RetryFunction5Ext" should "wrap a five-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean, Double, Char) => IO[String] = (input, number, flag, value, char) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag, $value, $char"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true, 3.14, 'A').unsafeRunSync()

    result shouldBe "Hello, 42, true, 3.14, A"
    attempts.get() shouldBe 3
  }

  "RetryFunction6Ext" should "wrap a six-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean, Double, Char, Long) => IO[String] = (input, number, flag, value, char, long) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag, $value, $char, $long"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true, 3.14, 'A', 100L).unsafeRunSync()

    result shouldBe "Hello, 42, true, 3.14, A, 100"
    attempts.get() shouldBe 3
  }

  "RetryFunction7Ext" should "wrap a seven-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean, Double, Char, Long, Float) => IO[String] = (input, number, flag, value, char, long, float) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag, $value, $char, $long, $float"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true, 3.14, 'A', 100L, 1.23f).unsafeRunSync()

    result shouldBe "Hello, 42, true, 3.14, A, 100, 1.23"
    attempts.get() shouldBe 3
  }

  "RetryFunction8Ext" should "wrap an eight-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean, Double, Char, Long, Float, Short) => IO[String] = (input, number, flag, value, char, long, float, short) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag, $value, $char, $long, $float, $short"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true, 3.14, 'A', 100L, 1.23f, 10.toShort).unsafeRunSync()

    result shouldBe "Hello, 42, true, 3.14, A, 100, 1.23, 10"
    attempts.get() shouldBe 3
  }

  "RetryFunction9Ext" should "wrap a nine-argument function with retry logic" in {
    val attempts = new AtomicInteger(0)
    val retryPlan = retry[IO](3)

    val function: (String, Int, Boolean, Double, Char, Long, Float, Short, Byte) => IO[String] = (input, number, flag, value, char, long, float, short, byte) => IO {
      if (attempts.incrementAndGet() < 3) throw new RuntimeException("Failure")
      s"$input, $number, $flag, $value, $char, $long, $float, $short, $byte"
    }

    val functionWithRetry = function.withRetry(retryPlan)
    val result = functionWithRetry("Hello", 42, true, 3.14, 'A', 100L, 1.23f, 10.toShort, 1.toByte).unsafeRunSync()

    result shouldBe "Hello, 42, true, 3.14, A, 100, 1.23, 10, 1"
    attempts.get() shouldBe 3
  }
}