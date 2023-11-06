package org.ldg.test

import org.scalacheck.Gen
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiniteGenSpec extends AnyFlatSpec with Matchers with Inside {
  import FiniteGen._

  "Gen.once([A])" should "generate a single value and then fail" in {
    val gen = FiniteGen.once("a")
    gen.sample shouldBe Some("a")
    gen.sample shouldBe None
  }

  def mkListGen(n: Int = 10, start: Int = 0): Gen[List[Int]] = Gen.listOfN(n, {
    var i = start
    Gen.delay {
      i = i + 1
      Gen.const(i - 1)
    }
  })

  "Gen.once(Gen[A])" should "generate a single value and then fail" in {
    val gen = FiniteGen.once(mkListGen())
    gen.sample shouldBe Some(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    gen.sample shouldBe None
  }

  "Gen.andThen" should "generate a values from a generator and then values from another generator" in {
    val gen1 = FiniteGen.once("a")
    val gen2 = FiniteGen.once("b")
    val gen = gen1.andThen(gen2)
    gen.sample shouldBe Some("a")
    gen.sample shouldBe Some("b")
    gen.sample shouldBe None
  }

  "Gen.allOf(Iterable)" should "generate all values and then fail" in {
    val gen = FiniteGen.allOf(Seq("a","b","c"))
    gen.sample shouldBe Some("a")
    gen.sample shouldBe Some("b")
    gen.sample shouldBe Some("c")
    gen.sample shouldBe None
  }

  "Gen.allOf(Gen[Iterable])" should "generate all values and then fail" in {
    val n = 10
    val listGen = mkListGen(n)
    val listTwiceGen = FiniteGen.once(listGen).andThen(FiniteGen.once(listGen))
    val gen = FiniteGen.flatten(listTwiceGen)
    for(i <- 0 until n * 2) {
      gen.sample shouldBe Some(i)
    }
    gen.sample shouldBe None
  }

  "Gen.zipConcat" should "alternate generator values from delegate generators" in {
    val n = 10
    val gen1 = FiniteGen.flatten(FiniteGen.once(mkListGen(n)))
    val gen2 = FiniteGen.flatten(FiniteGen.once(mkListGen(n, 100)))
    val gen = gen1.zipConcat(gen2)
    for(i <- 0 until n) {
      gen.sample shouldBe Some(i)
      gen.sample shouldBe Some(100 + i)
    }
    gen.sample shouldBe None
  }

  "FiniteGenExample" should "return expected results" in {
    val stringGen2 =
      FiniteGen.once("") andThen
      FiniteGen.once(Gen.asciiChar.map(_.toString)) andThen
      Gen.asciiStr

    stringGen2.sample shouldBe Some("")
    inside(stringGen2.sample) { case Some(result) =>
      result.length shouldBe 1
      (0 to 127).contains(result(0)) shouldBe true
    }

    def ensureAscii(s: String): Unit =
      for (c <- s) {
        (0 to 127).contains(c) shouldBe true
      }

    inside(stringGen2.sample) { case Some(result) =>
      ensureAscii(result)
    }
    inside(stringGen2.sample) { case Some(result) =>
      ensureAscii(result)
    }
    inside(stringGen2.sample) { case Some(result) =>
      ensureAscii(result)
    }
    // infinite...
  }

}
