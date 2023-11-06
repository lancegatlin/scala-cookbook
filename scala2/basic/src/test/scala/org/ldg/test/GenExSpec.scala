package org.ldg.test

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenExSpec extends AnyFlatSpec with Matchers {
  import GenEx._

  "Gen.once([A])" should "generate a single value and then fail" in {
    val gen = Gen.once("a")
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
    val gen = Gen.once(mkListGen())
    gen.sample shouldBe Some(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    gen.sample shouldBe None
  }

  "Gen.andThen" should "generate a values from a generator and then values from another generator" in {
    val gen1 = Gen.once("a")
    val gen2 = Gen.once("b")
    val gen = gen1.andThen(gen2)
    gen.sample shouldBe Some("a")
    gen.sample shouldBe Some("b")
    gen.sample shouldBe None
  }

  "Gen.allOf(Iterable)" should "generate all values and then fail" in {
    val gen = Gen.allOf(Seq("a","b","c"))
    gen.sample shouldBe Some("a")
    gen.sample shouldBe Some("b")
    gen.sample shouldBe Some("c")
    gen.sample shouldBe None
  }

  "Gen.allOf(Gen[Iterable])" should "generate all values and then fail" in {
    val n = 10
    val listGen = mkListGen(n)
    val listTwiceGen = Gen.once(listGen).andThen(Gen.once(listGen))
    val gen = Gen.allOf(listTwiceGen)
    for(i <- 0 until n * 2) {
      gen.sample shouldBe Some(i)
    }
    gen.sample shouldBe None
  }

  "Gen.zipConcat" should "alternate generator values from delegate generators" in {
    val n = 10
    val gen1 = Gen.allOf(Gen.once(mkListGen(n)))
    val gen2 = Gen.allOf(Gen.once(mkListGen(n, 100)))
    val gen = gen1.zipConcat(gen2)
    for(i <- 0 until n) {
      gen.sample shouldBe Some(i)
      gen.sample shouldBe Some(100 + i)
    }
    gen.sample shouldBe None
  }
}
