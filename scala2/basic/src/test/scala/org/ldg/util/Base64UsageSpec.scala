package org.ldg.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Base64UsageSpec extends AnyFlatSpec with Matchers {
  "Usage method 1 example" should "compile" in {
    object Usage1 {
      import org.ldg.util.StringExt.charset
      import org.ldg.util.Base64Id.Implicits.base64DefaultId // select which base64 encoding scheme to use
      import org.ldg.util.ByteCoderExt._
      val bytes: Array[Byte] = ???

      val encodedText = bytes.encodeBase64 // uses in scope implicit Base64[Id] instance (Rfc4648 by default)
      val decodedBytes = encodedText.decodeBase64
    }
    // no tests, ensure compile only
  }

  "Usage method 2 example" should "compile" in {
    object Usage2 {
      import cats.Id
      import org.ldg.util.StringExt.charset
      import org.ldg.util.Base64Id.Implicits.base64UrlSafeId
      import org.ldg.util.ByteCoderExt._
      val bytes: Array[Byte] = ???

      val encodedText2 = bytes.encode( Base64.UrlSafe[Id] )
      val decodedBytes2 = encodedText2.decode( Base64.UrlSafe[Id] )
    }
    // no tests, ensure compile only
  }

  "Usage method 3 example" should "compile" in {
    object Usage3 {
      import cats.Id
      import org.ldg.util.StringExt.charset
      import org.ldg.util.Base64Id.Implicits.base64UrlSafeId
      val bytes: Array[Byte] = ???

      val encodedText = Base64[Id].encode( bytes )
      val decodedBytes = Base64[Id].decode( encodedText )
    }
    // no tests, ensure compile only
  }

}