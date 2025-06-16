package org.ldg.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ldg.util.ByteArrayExt._

import java.nio.charset.{Charset, StandardCharsets}

class ByteArrayExtSpec extends AnyFlatSpec with Matchers {
  implicit val charset: Charset = StandardCharsets.UTF_8

  "Array[Byte].toStringFromBytes" should "act as syntactic-sugar for new String" in {
    val string = "Hello, World!"
    val bytes = string.getBytes(charset)
    bytes.toStringFromBytes shouldEqual string
  }

  "Array[Byte].toUtf8String" should "act as syntactic-sugar for new String" in {
    val string = "Hello, World!"
    val bytes = string.getBytes(charset)
    bytes.toUtf8String shouldBe string
  }

}
