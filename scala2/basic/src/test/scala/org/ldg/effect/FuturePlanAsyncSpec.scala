package org.ldg.effect

import cats.effect.Async
import cats.effect.implicits.monadCancelOps_
import cats.effect.kernel.{Outcome, Sync}
import org.ldg.concurrent.ExecutionContextExt.OrgLdgConcurrentExecutionExt
import org.ldg.concurrent.BlockingExecutionContext
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

// note: this spec also fully tests FuturePlanEval and FuturePlanFiber
class FuturePlanAsyncSpec extends AnyFlatSpec with Matchers with ScalaFutures with Eventually  with BaseFuturePlanSpec {

  "FuturePlanAsync" should "return a pure value" in {
    F.pure( 42 ).run().futureValue shouldBe 42
  }

  it should "map a FuturePlan" in {
    F.map( FuturePlan.defer( Future( 10 ) ) )( _ * 2 ).run().futureValue shouldBe 20
  }

  it should "flatMap a FuturePlan" in {
    F.flatMap( FuturePlan.delay( 5 ) )( x => FuturePlan.delay( x + 3 ) ).run().futureValue shouldBe 8
  }

  it should "correctly handle tail-recursive computations with tailRecM" in {
    val result = F
      .tailRecM[Int, Int]( 0 ) { i =>
        if (i < 5) FuturePlan.delay( Left( i + 1 ) )
        else FuturePlan.delay( Right( i ) )
      }.run().futureValue

    result shouldBe 5
  }

  it should "raise an error with raiseError" in {
    val result = F.raiseError[Int]( new RuntimeException( "Test Error" ) ).run().failed.futureValue
    result shouldBe a[RuntimeException]
    result.getMessage shouldBe "Test Error"
  }

  it should "handle errors with handleErrorWith" in {
    val error = FuturePlan.failed[Int]( new RuntimeException( "Error" ) )
    F.handleErrorWith( error )( _ => FuturePlan.delay( 99 ) ).run().futureValue shouldBe 99
  }

  it should "suspend a computation" in {
    F.suspend( Sync.Type.Delay ) {
        Thread.sleep( 100 ) // simulate some processing time
        42
      }.run().futureValue shouldBe 42
  }

  it should "return the execution context" in {
    F.executionContext.run().futureValue shouldBe ec
  }

  it should "provide evalOn to run everything on a single execution context" in {
    val flag = new AtomicBoolean( false )
    val customEC = ExecutionContext.fromExecutor { command =>
      flag.set( true )
      command.run()
    }
    F.evalOn( FuturePlan.delay( 10 ), customEC ).run().futureValue shouldBe 10
    flag.get() shouldBe true
  }

  it should "start a fiber" in {
    val fiber = F.start( FuturePlan.delay( 42 ) ).run().futureValue
    val Outcome.Succeeded( fa ) = fiber.join.run().futureValue
    fa.run().futureValue shouldBe 42
  }

  it should "cancel a fiber" in {
    val fiber = F.start(
      FuturePlan.delay {
        Thread.sleep(50) // simulate some processing time
        42
      }
        .map(_ * 2)
    ).run().futureValue

    (for {
      _ <- fiber.cancel
      result <- fiber.join
    } yield result).run().futureValue shouldBe Outcome.Canceled()
  }

  it should "join a fiber from another fiber" in {
    import cats.effect.implicits.genSpawnOps
    val plan = for {
      fiber <- FuturePlan.delay {
        Thread.sleep( 50 ) // simulate some processing time
        42
      }.start
      outcome <- fiber.join
      result <- outcome.fold(
        FuturePlan.pure(0),
        ex => throw ex,
        a => a.map(_ * 2)
      )
    } yield result
    plan.run().futureValue shouldBe 84
  }

  it should "join a fiber from another fiber and allow canceling that fiber" in {
    import cats.effect.implicits.genSpawnOps

    // maybe-do: turn off reporting unhandled exceptions for this test which intermittently detects fiber join late
    val plan = for {
      fiber <- FuturePlan.delay {
        Thread.sleep( 100 ) // simulate some processing time
        42
      }
        // add a step after the future completes to ensure the fiber checks cancel flag
        .map { i =>
          i * 2
        }
        .start
      _ <- fiber.cancel // cancel the fiber (before the 100 ms sleep finishes)
      outcome <- fiber.join
      result <- outcome.fold(
        FuturePlan.pure(0),
        ex => throw ex,
        a => a.map(_ * 2)
      )
    } yield result

    plan.run().futureValue shouldBe 0
  }

  it should "report an unhandled exception from a failed fiber that is never joined" in {
    import cats.effect.implicits.genSpawnOps
    val counter = new AtomicInteger(0)
    implicit val ec: ExecutionContext = ExecutionContext.global.withUnhandledExceptionHandler { _ =>
      counter.incrementAndGet()
    }
    implicit val bec: BlockingExecutionContext = BlockingExecutionContext(ec)
    implicit val F: Async[FuturePlan] = new FuturePlanAsync(
      FuturePlanAsync.Config(FuturePlanFiber.Config(
        maybeFailureJoinTimeout = Some(1.millis)
      ))
    )

    val fiber = FuturePlan.delay {
      throw new RuntimeException( "Fiber failed" )
    }.start.run().futureValue
    Thread.sleep(50) // give the fiber some time to detect there was never a join call
    counter.get() shouldBe 1 // should have reported the unhandled exception
    fiber.join.run().futureValue shouldBe a[Outcome.Errored[FuturePlan,_,_]]
  }

  it should "forceR should ignore failures in first computation but not if CancellationException" in {
    F.forceR( FuturePlan.failed( new RuntimeException ) )( FuturePlan.delay( 20 ) ).run().futureValue shouldBe 20
    F.forceR( F.canceled )( FuturePlan.delay( 20 ) ).run().failed.futureValue shouldBe a[CanceledEvalException]
  }

  it should "return a canceled computation" in {
    val plan =
      for {
        i <- FuturePlan.delay( 42 )
        _ <- F.canceled // this should cancel the computation
        result <- F.delay( i * 2 )
      } yield result

    val result = plan.run().failed.futureValue
    result shouldBe a[CanceledEvalException]
    result.getMessage shouldBe "FuturePlanEval evaluated FuturePlan.canceled"
  }

  it should "run onCancel finalizers" in {
    val counter = new AtomicInteger(0)

    val plan = (
      for {
        i <- FuturePlan.delay( 42 )
        // should not run this finalizer since its plan isn't canceled
        i2 <- F.onCancel( FuturePlan.delay( 42 ), FuturePlan.delay( counter.incrementAndGet() ) )
        _ <- F.canceled // this should cancel the computation
        result <- F.delay( i + i2 )
      } yield result
    )
      .onCancel( FuturePlan.delay( counter.incrementAndGet() ) )
      .onCancel( FuturePlan.delay( counter.incrementAndGet() ) )

    plan.run().failed.futureValue shouldBe a[CanceledEvalException]
    counter.get() shouldBe 2
  }

  it should "future should not complete before all finalizers run" in {
    val signal = new AtomicBoolean(false)
    val counter = new AtomicInteger(0)

    val plan = (
      for {
        i <- FuturePlan.delay( 42 )
        // should not run this finalizer since its plan isn't canceled
        i2 <- F.onCancel( FuturePlan.delay( 42 ), FuturePlan.delay( counter.incrementAndGet() ) )
        _ <- F.canceled // this should cancel the computation
        result <- F.delay( i + i2 )
      } yield result
      )
      .onCancel( FuturePlan.delay( counter.incrementAndGet() ) )
      .onCancel(FuturePlan.delay {
        while( !signal.get() ) { // simulate some processing time
          Thread.sleep( 1 )
        }
        counter.incrementAndGet()
      })

    val future = plan.run()
    future.isCompleted shouldBe false
    eventually {
      counter.get() shouldBe 1
    }
    counter.get() shouldBe 1
    signal.set(true)
    future.failed.futureValue shouldBe a[CanceledEvalException]
    counter.get() shouldBe 2
  }


  it should "create an uncancelable computation" in {
    F.uncancelable( _ =>
        for {
          result <- FuturePlan.delay( 42 )
          _ <- F.canceled // this should not cancel the computation
        } yield result ).run().futureValue shouldBe 42
  }

  it should "create an uncancelable computation but still allow canceling within a poll" in {
    val flag = new AtomicBoolean(false)
    F.uncancelable( poll =>
      for {
        result <- FuturePlan.delay( 42 )
        _ <- F.canceled // this should not cancel the computation
        _ = flag.set(true)
        _ <- poll(F.canceled) // this should cancel the computation
      } yield result ).run().failed.futureValue shouldBe a[CanceledEvalException]
    flag.get() shouldBe true
  }

  it should "return monotonic time" in {
    val now = System.nanoTime().nanos
    F.monotonic.run().futureValue >= now shouldBe true
  }

  it should "return real time" in {
    val now = System.currentTimeMillis().millis
    F.realTime.run().futureValue >= now shouldBe true
  }

  it should "sleep for a duration" in {
    val start = System.nanoTime()
    F.sleep( 100.millis ).run().futureValue
    val elapsed = (System.nanoTime() - start).nanos
    elapsed should be >= 100.millis
  }

  it should "cede execution" in {
    // not really testable, but we can check it runs without throwing an exception
    F.cede.run().futureValue shouldBe ()
  }

  it should "create a Ref" in {
    val ref = F.ref( 42 ).run().futureValue
    ref.set( 100 ).run().futureValue
    ref.get.run().futureValue shouldBe 100
  }

  it should "create a Deferred" in {
    val deferred = F.deferred[Int].run().futureValue
    deferred.complete( 42 ).run().futureValue
    deferred.get.run().futureValue shouldBe 42
  }

// maybe-do?
//  it should "run a continuation" in {
//  }
}
