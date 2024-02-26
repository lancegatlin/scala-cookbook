package org.ldg.util

import org.ldg.util.AnyTransformExt._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnyTransformExtSpec extends AnyFlatSpec with Matchers {
  case class Test( i: Int, s: String ) {
    def add( n: Int ): Test = copy( i = i + n )
    def addOne: Test = add( 1 )
  }

  "transform(function)" should "call transformer function and return modified self" in {
    val t = Test( 1, "abc123" )
    t.transform( _.addOne ) shouldBe Test( 2, "abc123" )
  }

  "transformIf(boolean)" should "call transformer function if test value is true and return modified self" in {
    val t = Test( 1, "abc123" )
    t.transformIf( false )( _.addOne ) shouldBe t
    t.transformIf( true )( _.addOne ) shouldBe Test( 2, "abc123" )
  }

  "transformIf(option)" should "call transformer function if Some and return modified self" in {
    val t = Test( 1, "abc123" )
    t.transformIf( Option.empty[Int] )( _.add( _ ) ) shouldBe t
    t.transformIf( Some( 123 ) )( _.add( _ ) ) shouldBe Test( 1 + 123, "abc123" )
  }

  "transformFold(collection)" should "call transformer for each collection value and return modified self" in {
    val t = Test( 1, "abc123" )
    t.transformFold( Seq.empty[Int] )( _.add( _ ) ) shouldBe t
    t.transformFold( Seq( 123, 1 ) )( _.add( _ ) ) shouldBe Test( 1 + 123 + 1, "abc123" )
  }
}
