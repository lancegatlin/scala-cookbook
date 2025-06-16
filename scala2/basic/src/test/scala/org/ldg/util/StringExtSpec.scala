package org.ldg.util

import org.ldg.util.StringExt._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringExtSpec extends AnyFlatSpec with Matchers {

  "String toBytes" should "act as syntactic-sugar for String.getBytes(charset)" in {
    val input = "Hello, World!"
    input.toBytes shouldBe input.getBytes(defaultCharset)
  }
}
