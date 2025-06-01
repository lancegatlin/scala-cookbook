package org.ldg.retry

import cats.effect.IO
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxMonadErrorRethrow, toFunctorOps}
import cats.{Applicative, Id}
import org.ldg.effect.{ClockForId, ClockForTry, clockForFuture}
import org.scalatest.Assertion
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Tests for examples of retrying effects using the Retry library (from documentation and also for demo).
  */
class RetryDemoSpec extends AnyFlatSpec with Matchers with ScalaFutures {
//  import RetryConfig.Sl4jLogging.Implicits.default
  implicit def retryConfig[F[_]: Applicative]: RetryConfig[F] =
    RetryConfig
      .NoLogging.default[F]().copy(
        calcRetryDelay = { ( _, _ ) =>
          10.millis.pure
        }
      )

  def createEffectWhichPassesOn3rdAttempt[F[_]: Applicative](): String => F[Either[Throwable, Int]] = {
    val attemptCount = new AtomicInteger( 0 )

    { _ =>
      if (attemptCount.incrementAndGet() == 3) {
        Right( attemptCount.get ).pure.widen
      } else {
        Left( new RuntimeException( s"Failed on attempt ${attemptCount.get()}" ) ).pure.widen
      }
    }
  }

  "retry[Future]" should "run an effect" in {
    implicit val executionContext: ExecutionContext = ExecutionContext.global

    val effect: String => Future[Int] =
      createEffectWhichPassesOn3rdAttempt[Future]().andThen( _.rethrow )

    retry( 5 )( () => effect( "example" ) ).futureValue shouldBe 3
  }

  "retry[IO]" should "run an effect" in {
    import cats.effect.unsafe.implicits.global

    val effect: String => IO[Int] =
      createEffectWhichPassesOn3rdAttempt[IO]().andThen( _.rethrow )

    retry( 5 )( () => effect( "example" ) ).unsafeRunSync() shouldBe 3
  }

  "retry[Try]" should "run an effect" in {
    val effect: String => Try[Int] =
      createEffectWhichPassesOn3rdAttempt[Try]().andThen( _.rethrow )

    retry( 5 )( () => effect( "example" ) ).get shouldBe 3
  }

  "retry[Id]" should "run an effect" in {
    val effect: String => Id[Either[Throwable, Int]] =
      createEffectWhichPassesOn3rdAttempt[Id]()

    retryCapture( 5 )( () => effect( "example" ) ).fold( throw _, identity ) shouldBe 3
  }

  def taglessRunAnEffectTest[F[_]: Retryable](): F[Assertion] = {
    val effect: String => F[Either[Throwable, Int]] =
      createEffectWhichPassesOn3rdAttempt[F]()

    retryCapture( 5 )( () => effect( "example" ) ).map( _ shouldBe Right( 3 ) )
  }

  "retry[F]" should "run an effect" in {
    import cats.effect.unsafe.implicits.global
    implicit val executionContext: ExecutionContext = ExecutionContext.global

    taglessRunAnEffectTest[IO]().unsafeRunSync()
    taglessRunAnEffectTest[Future]().futureValue
    taglessRunAnEffectTest[Try]()
    taglessRunAnEffectTest[Id]()
  }
  // todo: retry[F] with a timeout
  // todo: RetryPlan
  // todo: functionX.withRetry(retryPlan)

}
