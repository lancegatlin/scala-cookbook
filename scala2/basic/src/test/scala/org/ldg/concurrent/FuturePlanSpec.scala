package org.ldg.concurrent

import cats.implicits.catsSyntaxApplicativeError
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class FuturePlanSpec extends AnyFlatSpec with Matchers with ScalaFutures with BaseFuturePlanSpec { self =>
  def createLocalExecutionContext( f: () => Unit ): ExecutionContext = new ExecutionContext {
    override def execute( runnable: Runnable ): Unit = {
      f()
      self.ec.execute( runnable )
    }
    override def reportFailure( cause: Throwable ): Unit = self.ec.reportFailure( cause )
  }

  "FuturePlan" should "successfully create a pure FuturePlan with a value" in {
    val futurePlan = FuturePlan.pure( 42 )
    val result = futurePlan.run().futureValue
    result shouldBe 42
  }

  it should "have unit" in {
    val futurePlan = FuturePlan.unit
    futurePlan.run().futureValue shouldBe ()
  }

  it should "create failed FuturePlan that evals to failed future" in {
    val message = "error"
    val futurePlan = FuturePlan.failed( new RuntimeException( message ) )
    futurePlan.run().failed.futureValue.getMessage shouldBe message
  }

  it should "create FuturePlan from a successful try that evals to a future of the try" in {
    FuturePlan.fromTry( Success( 42 ) ).run().futureValue shouldBe 42
  }

  it should "create FuturePlan from a failed try that evals to a future of the try" in {
    val message = "error"
    FuturePlan
      .fromTry( Failure( new RuntimeException( message ) ) )
      .run()
      .failed
      .futureValue
      .getMessage shouldBe message
  }

  it should "create FuturePlan from a successful Right that evals to a future of the Either" in {
    FuturePlan.fromEither( Right( 42 ) ).run().futureValue shouldBe 42
  }

  it should "create FuturePlan from a failed Left that evals to a future of the Either" in {
    val message = "error"
    FuturePlan
      .fromEither( Left( new RuntimeException( message ) ) )
      .run()
      .failed
      .futureValue
      .getMessage shouldBe message
  }

  it should "evaluate a delayed computation" in {
    val futurePlan = FuturePlan.delay( 42 )
    val result = futurePlan.run().futureValue
    result shouldBe 42
  }

  it should "evaluate a deferred Future" in {
    val futurePlan = FuturePlan.defer( Future( 42 ) )
    val result = futurePlan.run().futureValue
    result shouldBe 42
  }

  it should "successfully map a FuturePlan to a new value" in {
    val futurePlan = FuturePlan.delay( 42 )
    val mappedPlan = futurePlan.map( _ * 2 )
    val result = mappedPlan.run().futureValue
    result shouldBe 84
  }

  it should "bind implicit ExecutionContext for map" in {
    val flag = new AtomicBoolean( false )
    implicit val ec: ExecutionContext = createLocalExecutionContext( () => flag.set( true ) )
    val futurePlan = FuturePlan.delay( 42 )
    val mappedPlan = futurePlan.map( _ * 2 )
    val result = mappedPlan.run().futureValue
    result shouldBe 84
    flag.get() shouldBe true
  }

  it should "flatMap a FuturePlan to another FuturePlan" in {
    val futurePlan = FuturePlan.delay( 42 )
    val flatMappedPlan = futurePlan.flatMap( x => FuturePlan.delay( x * 2 ) )
    val result = flatMappedPlan.run().futureValue
    result shouldBe 84
  }

  it should "bind implicit ExecutionContext for flatMap" in {
    val flag = new AtomicBoolean( false )
    implicit val ec: ExecutionContext = createLocalExecutionContext( () => flag.set( true ) )
    val futurePlan = FuturePlan.delay( 42 )
    val flatMappedPlan = futurePlan.flatMap( x => FuturePlan.delay( x * 2 ) )
    val result = flatMappedPlan.run().futureValue
    result shouldBe 84
    flag.get() shouldBe true
  }

  it should "handle errors using handleErrorWith" in {
    val futurePlan = FuturePlan.failed[Int]( new RuntimeException( "Error" ) )
    val recoveredPlan = futurePlan.handleErrorWith( _ => FuturePlan.delay( 42 ) )
    val result = recoveredPlan.run().futureValue
    result shouldBe 42
  }

  it should "cancel a FuturePlan and return a cancellation exception" in {
    val cancellablePlan = F.canceled
    val Left( result: CanceledEvalException ) = cancellablePlan.run().attempt.futureValue
    result.getMessage shouldBe "FuturePlan eval canceled"
  }

  it should "raise an error and propagate it through the FuturePlan" in {
    val futurePlan: FuturePlan[Int] = F.raiseError( new RuntimeException( "Error" ) )
    val Left( result: RuntimeException ) = futurePlan.run().attempt.futureValue
    result.getMessage shouldBe "Error"
  }

  "FuturePlan.evalOn" should "copy a FuturePlan for an ExecutionContext" in {
    val flag = new AtomicBoolean( false )
    val customEc: ExecutionContext = ExecutionContext.fromExecutor { command =>
      flag.set( true )
      command.run()
    }
    val futurePlan = FuturePlan.evalOn( FuturePlan.delay( 42 ), customEc )
    val result = futurePlan.run()( customEc, implicitly ).futureValue
    result shouldBe 42
    flag.get() shouldBe true
  }

}
