package org.ldg.util

import cats.Id
import org.ldg.util.Base64Id.Implicits.base64DefaultId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ldg.util.StringExt._
import org.ldg.util.ByteCoderExt._

class ByteCoderExtSpec extends AnyFlatSpec with Matchers {

  "encode/decode" should "act as syntactic-sugar for ByteCoder.encode/ByteCoder.decode" in {
    val input = "Hello, World!".toBytes
    val encoded = input.encode(Base64[Id])
    encoded should not be empty
    val decoded = encoded.decode(Base64[Id])
    decoded shouldEqual input
  }

  "encodeBase64/decodeBase64" should "act as syntactic-sugar for ByteCoder.encode/ByteCoder.decode" in {
    val input = "Hello, World!".toBytes
    val encoded = input.encodeBase64
    encoded should not be empty
    val decoded = encoded.decodeBase64
    decoded shouldEqual input
  }
}
