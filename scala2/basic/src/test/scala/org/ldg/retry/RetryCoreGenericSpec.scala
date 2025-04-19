package org.ldg.retry

import cats.{Applicative, Monad}
import org.ldg.effect.Sleep
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionException
import scala.concurrent.duration.{DurationInt, FiniteDuration}

abstract class RetryCoreGenericSpec[F[_]:Monad:Sleep] extends AnyFlatSpec with Matchers with Inside {
  import org.ldg.retry.RetryCore._

  protected def testRunSync[A](f: F[A], timeout: FiniteDuration = 10.seconds): A

  class Fixture(
    val maxAttempts: Int,
    val retryDelay: FiniteDuration
  ) {
    val capturedFailures = List.newBuilder[Throwable]

    val retryFixture: (Int => F[Either[Throwable, String]]) => F[(Int, Either[Throwable, String])] =
      runRetryState[F, Int, String](
        onBeforeAttempt = { attemptCount =>
          val newAttemptCount = attemptCount + 1
          capturedAttemptCounts += newAttemptCount
          Applicative[F].pure(newAttemptCount)
        },
        onAfterFailure = { (attemptCount, ex) =>
          capturedFailures += ex
          if (attemptCount < maxAttempts) {
            Applicative[F].pure((attemptCount, Some(retryDelay)))
          } else {
            Applicative[F].pure((attemptCount, None))
          }
        }
      )(
        initialState = 0
      )(_)

    val capturedAttemptCounts = List.newBuilder[Int]
    val effectSucceedsOn3rdAttempt: Int => F[Either[Throwable, String]] = { attemptCount: Int =>
      if (attemptCount < 3) {
        Applicative[F].pure(Left(new RuntimeException(s"failed $attemptCount")))
      } else {
        Applicative[F].pure(Right("success"))
      }
    }
  }


  "runRetryState" should "retry functions that fail when onAfterFailure returns Some" in new Fixture(5, 0.seconds) {
    testRunSync(retryFixture(effectSucceedsOn3rdAttempt)) shouldBe (3, Right("success"))
    capturedAttemptCounts.result() shouldBe List(1, 2, 3)
    capturedFailures.result().size shouldBe 2
  }

  it should "stop retrying functions that fail when onAfterFailure returns None" in new Fixture(2, 0.seconds) {
    inside(testRunSync(retryFixture(effectSucceedsOn3rdAttempt))) {
      case (2, Left(ex)) =>
        ex.getMessage shouldBe "failed 2"
        ex shouldBe a[RuntimeException]
    }
    capturedAttemptCounts.result() shouldBe List(1, 2)
    capturedFailures.result().size shouldBe 2
  }

  it should "delay retrying function that fail when onAfterFailure returns Some delay" in new Fixture(5, 250.millis) {
    val startTimeMs = System.currentTimeMillis()

    testRunSync(retryFixture(effectSucceedsOn3rdAttempt), retryDelay * 4) shouldBe (3, Right("success"))
    capturedAttemptCounts.result() shouldBe List(1, 2, 3)
    capturedFailures.result().size shouldBe 2
    val elapsedMs = System.currentTimeMillis() - startTimeMs
    elapsedMs should be >= retryDelay.toMillis
  }

  it should "stop retrying functions that fail with a fatal exception" in new Fixture(2, 0.seconds) {
    inside(testRunSync(retryFixture(_ =>
      Applicative[F].pure(Left(new ThreadDeath))
    ))) {
      case (1, Left(ex)) =>
        Option(ex.getCause) match {
          case Some(cause) =>
            // note: Future uses Java's Executors under the hood which wraps fatal exceptions in ExecutionException
            // but IO uses its own fibers code so it doesn't do this
            ex shouldBe a[ExecutionException]
            cause shouldBe a[ThreadDeath]
          case _ =>
            ex shouldBe a[ThreadDeath]
        }
    }
    capturedAttemptCounts.result() shouldBe List(1)
    capturedFailures.result().size shouldBe 0
  }
}