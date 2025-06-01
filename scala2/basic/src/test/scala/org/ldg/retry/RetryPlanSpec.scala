package org.ldg.retry

import cats.Applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApplicativeId
import org.ldg.retry.RetryConfig.NoLogging.Implicits.default
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class RetryPlanSpec extends AnyFlatSpec with Matchers {

  "retry with max attempts" should "retry the specified number of times and succeed" in {
    val attempts = new AtomicInteger( 0 )
    val maybePfCalcRetryDelay: PartialFunction[( RetryState, Throwable ), IO[FiniteDuration]] = { case ( _: RetryState, _: Throwable ) => 0.millis.pure[IO] }

    val retryPlan = RetryPlan[IO](
      maxAttempts = 3,
      maybePfCalcRetryDelay = Some( maybePfCalcRetryDelay )
    )

    val result = retryPlan
      .run { () =>
        IO {
          val newAttempts = attempts.addAndGet( 1 )
          if (newAttempts < 3) throw new RuntimeException( "Failure" )
          "Success"
        }
      }.unsafeRunSync()

    result shouldBe "Success"
    attempts.get shouldBe 3
  }

  "retry with max attempts" should "stop retrying after exceeding max attempts" in {
    val attempts = new AtomicInteger( 0 )
    val retryPlan = RetryPlan[IO]( maxAttempts = 2 )
    val alwaysFalse = false

    val result = retryPlan
      .run { () =>
        IO {
          attempts.incrementAndGet()
          if (alwaysFalse) {
            0
          } else {
            throw new RuntimeException( "Failure" )
          }
        }
      }.attempt.unsafeRunSync()

    result.isLeft shouldBe true
    attempts.get shouldBe 2
  }

  "retry with max attempts and max elapsed time" should "stop retrying after exceeding max elapsed time" in {
    val attempts = new AtomicInteger( 0 )
    val retryPlan = RetryPlan[IO](
      maxAttempts = 5,
      maybeMaxElapsedDuration = Some( 50.millis )
    )
    val alwaysFalse = false

    val result = retryPlan
      .run { () =>
        IO {
          attempts.incrementAndGet()
          Thread.sleep( 30 )
          if (alwaysFalse) {
            0
          } else {
            throw new RuntimeException( "Failure" )
          }
        }
      }.attempt.unsafeRunSync()

    result.isLeft shouldBe true
    attempts.get shouldBe 1
  }

  "retry with custom shouldRetry" should "not retry for unretryable exceptions" in {
    val attempts = new AtomicInteger( 0 )
    val retryPlan = RetryPlan[IO]( maxAttempts = 3 ).shouldRetry {
      case ( _, _: IllegalArgumentException ) => Applicative[IO].pure( false )
    }

    val result = retryPlan
      .run { () =>
        IO {
          attempts.incrementAndGet()
          throw new IllegalArgumentException( "Unretryable" )
        }
      }.attempt.unsafeRunSync()

    result.isLeft shouldBe true
    attempts.get shouldBe 1
  }

  "retry with custom retry delay" should "apply the specified delay between retries" in {
    val attempts = new AtomicInteger( 0 )
    val retryPlan = RetryPlan[IO]( maxAttempts = 3 ).calcRetryDelay { case ( _, _ ) => Applicative[IO].pure( 10.millis ) }

    val startTime = System.nanoTime()
    val result = retryPlan
      .run { () =>
        IO {
          attempts.incrementAndGet()
          if (attempts.get < 3) throw new RuntimeException( "Failure" )
          "Success"
        }
      }.unsafeRunSync()
    val elapsedTime = (System.nanoTime() - startTime).nanos

    result shouldBe "Success"
    attempts.get shouldBe 3
    elapsedTime.toMillis should be >= 20L
  }
}
