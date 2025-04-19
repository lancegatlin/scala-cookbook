package org.ldg.effect

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.implicits.{global => globalIORuntime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.{global => globalExecutionContext}
import scala.concurrent.Future
import scala.util.{Failure, Success}

class CaptureOrRunEffectSpec extends AnyFlatSpec with Matchers {

  "CaptureOrRunEffect" should "capture effect for Id monad" in {
    val result: Id[Int] = CaptureOrRunEffect[Id].effect( 42 )
    result shouldBe 42
  }

  it should "capture effect for Future monad" in {
    val result: Future[Int] = CaptureOrRunEffect[Future].effect( 42 )
    result.onComplete {
      case Success( value ) => value shouldBe 42
      case Failure( _ )     => fail( "Future failed" )
    }
  }

  it should "capture effect for IO monad" in {
    val result: IO[Int] = CaptureOrRunEffect[IO].effect( 42 )
    result.unsafeRunSync() shouldBe 42
  }

  it should "handle exception for Id monad" in {
    val result: Id[Either[Throwable, Int]] = CaptureOrRunEffect[Id].attempt( throw new RuntimeException( "error" ) )
    result shouldBe a[Left[_, _]]
  }

  it should "handle exception for Future monad" in {
    val result: Future[Either[Throwable, Int]] = CaptureOrRunEffect[Future].attempt( throw new RuntimeException( "error" ) )
    result.onComplete {
      case Success( value ) => value shouldBe a[Left[_, _]]
      case Failure( _ )     => fail( "Future failed" )
    }
  }

  it should "handle exception for IO monad" in {
    val result: IO[Either[Throwable, Int]] = CaptureOrRunEffect[IO].attempt( throw new RuntimeException( "error" ) )
    result.unsafeRunSync() shouldBe a[Left[_, _]]
  }
}
