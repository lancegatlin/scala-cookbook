package org.ldg.util.retry

import cats.{Applicative, Id}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class RetryStateSpec extends AnyFlatSpec with Matchers with Inside {
  class Fixture {
    implicit val now: Instant = Instant.now()

    val eventsBuffer = scala.collection.mutable.ListBuffer.empty[RetryEvent]
    implicit val retryConfig: RetryConfig[Id] = RetryConfig.default[Id]
      .copy(
        onRetryEvent = { event =>
          eventsBuffer += event
          // note: compiler can't infer type here without Applicative
          // ()
          Applicative[Id].pure(())
        }
      )
  }

  val correlationId = "test-correlation-id"

  "initialState" should "initialize to the initial retry state" in new Fixture {
    val maxAttempts = 3
    val maybeMaxElapsedTime = Some(1.minute)

    val initialState = RetryState.initial(maxAttempts, maybeMaxElapsedTime, correlationId)

    initialState.correlationId shouldBe correlationId
    initialState.attemptCount shouldBe 0
    initialState.maxAttempts shouldBe maxAttempts
    initialState.startTime shouldBe now
    initialState.maybeMaxElapsedTime shouldBe maybeMaxElapsedTime
    initialState.lastModified shouldBe now
    initialState.maybeLastRetryEvent shouldBe None

    eventsBuffer.result().size shouldBe 0
  }

  "elapsed" should "return elapsed time" in new Fixture {
    val state0 = RetryState.initial(3, Some(1.minute), correlationId)
    state0.elapsed(now.plusSeconds(3)) shouldBe 3.seconds
  }

  "lastModifiedElapsed" should "return elapsed time since last modified" in new Fixture {
    val state0 = RetryState.initial(3, Some(1.minute), correlationId)
    state0.lastModifiedElapsed shouldBe 0.seconds
    state0.onBeforeAttempt[Id](implicitly, now.plusSeconds(2)).lastModifiedElapsed shouldBe 2.seconds
  }

  "onBeforeAttempt" should "increment the attempt count and update the last modified time" in new Fixture {
    val initialState = RetryState.initial(3, None, "test-correlation-id")

    val updatedState = initialState.onBeforeAttempt[Id]

    updatedState.attemptCount shouldBe 1
    updatedState.lastModified shouldBe now
    initialState.maybeLastRetryEvent shouldBe None

    eventsBuffer.result().size shouldBe 0
  }

  val retryableRuntimeException = new RuntimeException("Retryable error")

  "onAfterFailure" should "return updated state and retry delay when retry is allowed" in new Fixture {
    val state0 = RetryState.initial(3, None, correlationId)

    // attempt #1
    val state1 = state0.onBeforeAttempt[Id]
    state1.attemptCount shouldBe 1
    val (state2, Some(retryDelay)) = state1.onAfterFailure[Id](retryableRuntimeException)
    val expectedRetryEvent1 = RetryEvent.OnAfterFailure(
      state1,
      retryableRuntimeException,
      RetryAction.AttemptAgain(retryDelay)
    )
    state2.maybeLastRetryEvent shouldBe Some(expectedRetryEvent1)

    state2.lastModified shouldBe now
    retryDelay should be > Duration.Zero

    val events = eventsBuffer.result()
    events.size shouldBe 1
    events(0) shouldBe expectedRetryEvent1
  }

  it should "return None/ExhaustedMaxAttempts when max attempts are exhausted" in new Fixture {
    val state0 = RetryState.initial(2, Some(1.minute), correlationId)
    // attempt #1
    val state1 = state0.onBeforeAttempt[Id]
    val (state2, Some(retryDelay)) = state1.onAfterFailure[Id](retryableRuntimeException)
    val expectedRetryEvent1 = RetryEvent.OnAfterFailure(state1, retryableRuntimeException, RetryAction.AttemptAgain(retryDelay))
    state2.maybeLastRetryEvent shouldBe Some(expectedRetryEvent1)
    // attempt #2
    val state3 = state2.onBeforeAttempt[Id]
    val (state4, None) = state3.onAfterFailure[Id](retryableRuntimeException)(implicitly, implicitly, now.plusSeconds(3))
    val expectedRetryEvent2 = RetryEvent.OnAfterFailure(
      state3,
      retryableRuntimeException,
      RetryAction.StopRetry(FailedRetryReason.ExhaustedMaxAttempts)
    )
    state4.maybeLastRetryEvent shouldBe Some(expectedRetryEvent2)

    val events = eventsBuffer.result()
    events.size shouldBe 2
    events(0) shouldBe expectedRetryEvent1
    events(1) shouldBe expectedRetryEvent2
  }

  val unretryableException = new IllegalArgumentException("Unretryable error")

  it should "return None/UnretryableFailure when the exception is unretryable" in new Fixture {
    val state0 = RetryState.initial(3, None, correlationId)
    val state1 = state0.onBeforeAttempt[Id]
    val (state2, None) = state1.onAfterFailure[Id](unretryableException)(implicitly, retryConfig, now.plusSeconds(3))
    val expectedRetryEvent1 = RetryEvent.OnAfterFailure(
      state1,
      unretryableException,
      RetryAction.StopRetry(FailedRetryReason.UnretryableFailure)
    )
    state2.maybeLastRetryEvent shouldBe Some(expectedRetryEvent1)

    val events = eventsBuffer.result()
    events.size shouldBe 1
    events(0) shouldBe expectedRetryEvent1
  }

  it should "return None/ExceededMaxElapsedTime when max elapsed time is exceeded" in new Fixture {
    val state0 = RetryState.initial(3, Some(1.second), correlationId)
    val now1 = now.plusSeconds(2)
    val state1 = state0.onBeforeAttempt[Id](implicitly, now1)
    state1.elapsed(now1) shouldBe 2.seconds
    val (state2, None) = state1.onAfterFailure[Id](retryableRuntimeException)(implicitly, retryConfig, now.plusSeconds(3))
    val expectedRetryEvent1 = RetryEvent.OnAfterFailure(
      state1,
      retryableRuntimeException,
      RetryAction.StopRetry(FailedRetryReason.ExceededMaxElapsedTime)
    )
    state2.maybeLastRetryEvent shouldBe Some(expectedRetryEvent1)

    val events = eventsBuffer.result()
    events.size shouldBe 1
    events(0) shouldBe expectedRetryEvent1
  }

  "onRetrySuccess" should "trigger a retry success event if there were retries" in new Fixture {
    val state0 = RetryState.initial(3, Some(1.minute), correlationId)
    state0.attemptCount shouldBe 0
    val state1 = state0.onBeforeAttempt[Id]
    state1.attemptCount shouldBe 1
    // note: throw away this state since if it "happened" we'd be done
    state1.onRetrySuccess[Id]()

    eventsBuffer.result().size shouldBe 0

    val (state2, Some(_)) = state1.onAfterFailure[Id](retryableRuntimeException)

    val state3 = state2.onBeforeAttempt[Id]
    state3.attemptCount shouldBe 2

    val state4 = state3.onRetrySuccess[Id]()

    val events = eventsBuffer.result()
    events.size shouldBe 2
    events(1) shouldBe RetryEvent.OnRetrySuccess(state3)
  }

  "fmtRetryState" should "format RetryState for logging" in new Fixture {
    val state0 = RetryState.initial(3, Some(1.minute), correlationId)
    RetryState.fmtRetryState(state0) shouldBe
      s"attempts=0/3 elapsed=0 milliseconds/60000 milliseconds correlationId=$correlationId"

    val state1 = state0.onBeforeAttempt[Id](implicitly, now.plusSeconds(2))
    RetryState.fmtRetryState(state1) shouldBe
      s"attempts=1/3 elapsed=2000 milliseconds/60000 milliseconds correlationId=$correlationId"

    val state0_2 = RetryState.initial(3, None, correlationId)
    val state1_2 = state0_2.onBeforeAttempt[Id](implicitly, now.plusSeconds(2))
    RetryState.fmtRetryState(state1_2) shouldBe
      s"attempts=1/3 elapsed=2000 milliseconds/inf correlationId=$correlationId"

    val state0_3 = RetryState.initial(Int.MaxValue, Some(30000.millis), correlationId)
    val state1_3 = state0_3.onBeforeAttempt[Id](implicitly, now.plusSeconds(3))
    RetryState.fmtRetryState(state1_3) shouldBe
      s"attempts=1/inf elapsed=3000 milliseconds/30000 milliseconds correlationId=$correlationId"

  }

}