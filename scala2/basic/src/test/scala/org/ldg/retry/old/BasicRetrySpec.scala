package org.ldg.retry.old

import org.ldg
import org.scalatest.Inside
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, DurationInt}

class BasicRetrySpec extends AnyFreeSpec with Matchers with ScalaFutures with Inside {
  import org.ldg.retry.old.BasicRetry._

  object ExampleUsage {
    import scala.concurrent.ExecutionContext.Implicits.global

    val doEffect: String => Future[String] = ???

    retry(5)(doEffect("example"))

    retry(1.minute)(doEffect("example"))

    retry(5)
      .shouldRetry {
        case _:RuntimeException => false
      }
      .onRetryEvent {
        case RetryEvent.OnAfterFailure(_, cause, RetryEvent.AttemptAgain(_)) =>
          println("retrying", cause)
        case e => RetryConfig.default.onRetryEvent(e)
      }
      .retryDelay((_, _) => 5.seconds)
      .run(doEffect("example"))

    import Implicits._
    val retryPlan =
      retry(5)
        .shouldRetry {
          case _: RuntimeException => false
        }

    val doEffectWithRetry: String => Future[String] = doEffect.withRetry(retryPlan)
    val doEffectWithRetry2: String => Future[String] = doEffect.withRetry(retry(5))
  }

  override implicit def patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = scaled(Span(5, Seconds)),
      interval = scaled(Span(20, Seconds))
    )

  def mkEffectSucceedsOn3rdAttempt(captureAttemptCount: Int => Unit): Int => Future[String] =
    { attemptCount: Int =>
      captureAttemptCount(attemptCount)
      if (attemptCount < 3) {
        Future.failed(new RuntimeException(s"failed $attemptCount"))
      } else {
        Future.successful("success")
      }
    }

  "retry" - {
    class Fixture {
      val capturedAttemptCounts = List.newBuilder[Int]
      val effectSucceedsOn3rdAttempt = mkEffectSucceedsOn3rdAttempt(capturedAttemptCounts.addOne)
    }

    "should retry a function that fails until it succeeds" in new Fixture {
      val result = retry(5)
        .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
        .futureValue

      result shouldBe "success"
      capturedAttemptCounts.result() shouldBe List(1,2,3)
    }

    "should stop retrying a function that exceeds max attempts" in new Fixture {
      val ex = retry(2)
        .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
        .failed
        .futureValue

      ex shouldBe a[RuntimeException]
      ex.getMessage shouldBe "failed 2"
      capturedAttemptCounts.result() shouldBe List(1,2)
    }

    "should stop retrying a function that exceeds max elapsed time" in new Fixture {
      val ex = retry(150.millis)
        .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
        .failed
        .futureValue

      ex shouldBe a[RuntimeException]
      ex.getMessage should startWith("failed")
      capturedAttemptCounts.result().size should be >= 1
    }

    "should not retry a function that throws an unretryable exception" in new Fixture {
      val ex = retry(5)
        .shouldRetry {
          case _:RuntimeException => false
        }
        .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
        .failed
        .futureValue

      ex shouldBe a[RuntimeException]
      ex.getMessage shouldBe "failed 1"
      capturedAttemptCounts.result() shouldBe List(1)
    }


    class EventCapturingFixture extends Fixture {
      val capturedEvents = List.newBuilder[RetryEvent]
    }

    "should allow overriding handling of RetryEvents and generate" - {

      "no RetryEvents when function succeeds on first attempt" in new EventCapturingFixture {
        val result = retry(5)
          .onRetryEvent(capturedEvents.addOne)
          .run { retryState =>
            capturedAttemptCounts += retryState.attemptCount
            Future.successful("success")
          }
          .futureValue

        result shouldBe "success"
        capturedAttemptCounts.result() shouldBe List(1)
        val events = capturedEvents.result()
        events.size shouldBe 0
      }

      "OnAfterFailure after each function call that fails and after success one OnRetrySuccess event" in new EventCapturingFixture {
        val result = retry(5)
          .onRetryEvent(capturedEvents.addOne)
          .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
          .futureValue

        result shouldBe "success"
        capturedAttemptCounts.result() shouldBe List(1, 2, 3)
        val events = capturedEvents.result()
        events.size shouldBe 3
        inside(events) { case List(
        _1@RetryEvent.OnAfterFailure(_, _: RuntimeException, RetryEvent.AttemptAgain(_)),
        _2@RetryEvent.OnAfterFailure(_, _: RuntimeException, RetryEvent.AttemptAgain(_)),
        _3@RetryEvent.OnRetrySuccess(_)
        ) =>
          _1.retryState.attemptCount shouldBe 1
          _2.retryState.attemptCount shouldBe 2
          _3.retryState.attemptCount shouldBe 3
        }
      }

      "OnAfterFailure RethrowFailure ExhaustedMaxAttempts after retrying a function exceeds max attempts" in new EventCapturingFixture {
        val ex = retry(2)
          .onRetryEvent(capturedEvents.addOne)
          .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
          .failed
          .futureValue

        ex shouldBe a[RuntimeException]
        ex.getMessage shouldBe "failed 2"
        capturedAttemptCounts.result() shouldBe List(1, 2)
        val events = capturedEvents.result()
        events.size shouldBe 2
        inside(events) { case List(
        _1@RetryEvent.OnAfterFailure(_, _: RuntimeException, RetryEvent.AttemptAgain(_)),
        _2@RetryEvent.OnAfterFailure(_, _: RuntimeException, RetryEvent.RethrowFailure(RetryEvent.ExhaustedMaxAttempts)),
        ) =>
          _1.retryState.attemptCount shouldBe 1
          _2.retryState.attemptCount shouldBe 2
        }
      }

      "OnAfterFailure RethrowFailure ExhaustedMaxElapsedTime after retrying a function exceeds max elapsed time" in new EventCapturingFixture {
        val ex = retry(1.nano)
          .onRetryEvent(capturedEvents.addOne)
          .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
          .failed
          .futureValue

        ex shouldBe a[RuntimeException]
        ex.getMessage shouldBe "failed 1"
        capturedAttemptCounts.result().size shouldBe 1
        val events = capturedEvents.result()
        events.size shouldBe 1
        inside(events) { case List(
        _1@RetryEvent.OnAfterFailure(_, _: RuntimeException, RetryEvent.RethrowFailure(RetryEvent.ExceededMaxElapsedTime)),
        ) =>
          _1.retryState.attemptCount shouldBe 1
        }
      }

      "OnAfterFailure RethrowFailure UnretryableException after a function throws an unretryable exception" in new EventCapturingFixture {
        val ex = retry(5)
          .onRetryEvent(capturedEvents.addOne)
          .shouldRetry {
            case _: RuntimeException => false
          }
          .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
          .failed
          .futureValue

        ex shouldBe a[RuntimeException]
        ex.getMessage shouldBe "failed 1"
        capturedAttemptCounts.result() shouldBe List(1)
        val events = capturedEvents.result()
        events.size shouldBe 1
        inside(events) { case List(
        _1@RetryEvent.OnAfterFailure(_, _: RuntimeException, RetryEvent.RethrowFailure(RetryEvent.UnretryableFailure)),
        ) =>
          _1.retryState.attemptCount shouldBe 1
        }
      }
    }

    "should allow overriding how to compute retry delay" in new EventCapturingFixture {
      val expectedRetryDelay = 123.nanos
      val result = retry(5)
        .onRetryEvent(capturedEvents.addOne)
        .retryDelay((retryState,_) => expectedRetryDelay * retryState.attemptCount)
        .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
        .futureValue

      result shouldBe "success"
      capturedAttemptCounts.result() shouldBe List(1, 2, 3)
      val events = capturedEvents.result()
      events.size shouldBe 3
      inside(events) { case List(
        _1@RetryEvent.OnAfterFailure(_, _, RetryEvent.AttemptAgain(retryDelay1)),
        _2@RetryEvent.OnAfterFailure(_, _, RetryEvent.AttemptAgain(retryDelay2)),
        _3@RetryEvent.OnRetrySuccess(_)
      ) =>
        _1.retryState.attemptCount shouldBe 1
        _1.cause shouldBe a[RuntimeException]
        retryDelay1 shouldBe expectedRetryDelay
        _2.retryState.attemptCount shouldBe 2
        _2.cause shouldBe a[RuntimeException]
        retryDelay2 shouldBe expectedRetryDelay * 2
        _3.retryState.attemptCount shouldBe 3
      }
    }

    "should allow saving the RetryPlan" in new EventCapturingFixture {
      val expectedRetryDelay = 123.nanos
      val fixture = retry(5)
        .shouldRetry {
          case ex:RuntimeException if ex.getMessage == "failed 2" => false
        }
        .onRetryEvent(capturedEvents.addOne)
        .retryDelay((_,_) => expectedRetryDelay)

      val ex = fixture
        .run(state => effectSucceedsOn3rdAttempt(state.attemptCount))
        .failed
        .futureValue

      ex shouldBe a[RuntimeException]
      ex.getMessage shouldBe "failed 2"
      capturedAttemptCounts.result() shouldBe List(1,2)
      val events = capturedEvents.result()
      events.size shouldBe 2
      inside(events) { case List(
      _1@RetryEvent.OnAfterFailure(_, _, RetryEvent.AttemptAgain(retryDelay1)),
      _2@RetryEvent.OnAfterFailure(_, _, RetryEvent.RethrowFailure(RetryEvent.UnretryableFailure))
      ) =>
        _1.retryState.attemptCount shouldBe 1
        _1.cause shouldBe a[RuntimeException]
        retryDelay1 shouldBe expectedRetryDelay
        _2.retryState.attemptCount shouldBe 2
        _2.cause shouldBe a[RuntimeException]
      }
    }

    "should allow converting RetryPlan to RetryConfig" in new EventCapturingFixture {
      val expectedRetryDelay = 123.nanos
      val expectedCorrelationId = "abc123"
      val retryPlan = retry(5)
        .shouldRetry {
          case ex:RuntimeException if ex.getMessage == "failed 2" => false
        }
        .onRetryEvent(capturedEvents.addOne)
        .retryDelay((_,_) => expectedRetryDelay)
        .correlationId(expectedCorrelationId)
      val fixture = retryPlan.toRetryConfig

      fixture.genCorrelationId() shouldBe expectedCorrelationId

      fixture.shouldRetry(new RuntimeException) shouldBe true
      fixture.shouldRetry(new RuntimeException("failed 2")) shouldBe false

      val fakeState = RetryState.initial(
        maxAttempts = 1,
        maxElapsedTime = Duration.Inf,
        correlationId = fixture.genCorrelationId()
      )(fixture)

      fixture.retryDelay(fakeState, new RuntimeException) shouldBe expectedRetryDelay
      val fakeEvent = RetryEvent.OnRetrySuccess(fakeState)
      fixture.onRetryEvent(fakeEvent)

      capturedEvents.result() shouldBe List(fakeEvent)
    }
  }


  // todo: fatal exception
  // todo: recurseUltimateCause
  // todo: correlationId
  // todo: RetryPlan.apply
  // todo: fmtRetryState
  // todo: loggingRetryEventHandler
  // todo: withRetry
}
