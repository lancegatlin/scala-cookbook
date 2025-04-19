package org.ldg.effect

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.implicits.{global => globalIORuntime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.{global => globalExecutionContext}
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class EffectSpec extends AnyFlatSpec with Matchers {

  "Effect" should "capture effect for Id monad" in {
    val result: Id[Int] = Effect[Id].effect( 42 )
    result shouldBe 42
  }

  it should "capture effect for Future monad" in {
    val result: Future[Int] = Effect[Future].effect( 42 )
    result.onComplete {
      case Success( value ) => value shouldBe 42
      case Failure( _ )     => fail( "Future failed" )
    }
  }

  it should "capture effect for IO monad" in {
    val result: IO[Int] = Effect[IO].effect( 42 )
    result.unsafeRunSync() shouldBe 42
  }

  it should "create Id monad from pure" in {
    val result: Id[Int] = Effect[Id].pure( 42 )
    result shouldBe 42
  }

  it should "handle flatMap for Id monad" in {
    val result: Id[Int] = Effect[Id].flatMap( Effect[Id].pure( 42 ) )( x => Effect[Id].pure( x + 1 ) )
    result shouldBe 43
  }

  it should "handle exception for Id monad" in {
    val result: Id[Either[Throwable, Int]] = Effect[Id].attempt( throw new RuntimeException( "error" ) )
    result shouldBe a[Left[_, _]]
  }

  it should "handle exception for Future monad" in {
    val result: Future[Either[Throwable, Int]] = Effect[Future].attempt( throw new RuntimeException( "error" ) )
    result.onComplete {
      case Success( value ) => value shouldBe a[Left[_, _]]
      case Failure( _ )     => fail( "Future failed" )
    }
  }

  it should "handle exception for IO monad" in {
    val result: IO[Either[Throwable, Int]] = Effect[IO].attempt( throw new RuntimeException( "error" ) )
    result.unsafeRunSync() shouldBe a[Left[_, _]]
  }

  it should "return current monotonic time for Id monad" in {
    val result: Id[FiniteDuration] = Effect[Id].monotonic
    result shouldBe a[FiniteDuration]
  }

  it should "return current real time for Id monad" in {
    val result: Id[FiniteDuration] = Effect[Id].realTime
    result shouldBe a[FiniteDuration]
  }

  it should "return current monotonic time for Future monad" in {
    val result: Future[FiniteDuration] = Effect[Future].monotonic
    result.onComplete {
      case Success( value ) => value shouldBe a[FiniteDuration]
      case Failure( _ )     => fail( "Future failed" )
    }
  }

  it should "return current real time for Future monad" in {
    val result: Future[FiniteDuration] = Effect[Future].realTime
    result.onComplete {
      case Success( value ) => value shouldBe a[FiniteDuration]
      case Failure( _ )     => fail( "Future failed" )
    }
  }

  it should "return current monotonic time for IO monad" in {
    val result: IO[FiniteDuration] = Effect[IO].monotonic
    result.unsafeRunSync() shouldBe a[FiniteDuration]
  }

  it should "return current real time for IO monad" in {
    val result: IO[FiniteDuration] = Effect[IO].realTime
    result.unsafeRunSync() shouldBe a[FiniteDuration]
  }
}
