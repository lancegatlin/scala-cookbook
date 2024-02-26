package org.ldg.util

import org.ldg.util.AnyTapExt._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnyTapExtSpec extends AnyFlatSpec with Matchers {
  case class Test( i: Int, s: String )

  "tap(function)" should "call a side-effect and return unmodified self" in {
    val t = Test( 1, "abc123" )
    var s = false
    (t.tap( _ => s = true ) eq t) shouldBe true
    s shouldBe true
  }

  "tapIf(boolean)" should "call a side-effect if test value is true and return unmodified self" in {
    val t = Test( 1, "abc123" )
    var s = false
    (t.tapIf( false )( _ => s = true ) eq t) shouldBe true
    s shouldBe false
    (t.tapIf( true )( _ => s = true ) eq t) shouldBe true
    s shouldBe true
  }

  "tapIf(option)" should "call a side-effect if Some and return unmodified self" in {
    val t = Test( 1, "abc123" )
    var s = 0
    (t.tapIf( Option.empty[Int] )( ( t, j ) => s += t.i + j ) eq t) shouldBe true
    s shouldBe 0
    (t.tapIf( Some( 123 ) )( ( t, j ) => s += t.i + j ) eq t) shouldBe true
    s shouldBe (1 + 123)
  }

  "tapForeach(collection)" should "call a side-effect for each collection value and return unmodified self" in {
    val t = Test( 1, "abc123" )
    var s = 0
    (t.tapForeach( Seq.empty[Int] )( ( t, j ) => s += t.i + j ) eq t) shouldBe true
    s shouldBe 0
    (t.tapForeach( Seq( 123, 1 ) )( ( t, j ) => s += t.i + j ) eq t) shouldBe true
    s shouldBe (1 + 123) + (1 + 1)
  }
}
