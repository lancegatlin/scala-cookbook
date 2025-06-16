package org.ldg.util

import cats.Id

import java.nio.charset.Charset

/**
 * Base64 encoding and decoding for the `Id` effect type (i.e. no effect capture).
 */
object Base64Id {
  object Implicits {
    // note: import only one of these implicits at a time, depending on the desired Base64 encoding scheme
    implicit val base64DefaultId: Base64[Id] =
      new Rfc4648
    implicit val base64Rfc4648Id: Base64.Rfc4648[Id] =
      new Rfc4648
    implicit val base64UrlSafeId: Base64.UrlSafe[Id] =
      new UrlSafe
    implicit val base64MimeId: Base64.Mime[Id] =
      new Mime
  }

  class Rfc4648 extends Base64.Rfc4648[Id] {
    override def encode(bytes: Array[Byte])(implicit charset: Charset): Id[String] =
      // note: could use encodeToString here, but it does not allow specifying charset
      new String(java.util.Base64.getEncoder.encode(bytes), charset)

    override def decode(encodedText: String): Id[Array[Byte]] =
      java.util.Base64.getDecoder.decode(encodedText)
  }

  class UrlSafe extends Base64.UrlSafe[Id] {
    override def encode(bytes: Array[Byte])(implicit charset: Charset): Id[String] =
      new String(java.util.Base64.getUrlEncoder.encode(bytes), charset)

    override def decode(encodedText: String): Id[Array[Byte]] =
      java.util.Base64.getUrlDecoder.decode(encodedText)
  }

  class Mime extends Base64.Mime[Id] {
    override def encode(bytes: Array[Byte])(implicit charset: Charset): Id[String] =
      new String(java.util.Base64.getMimeEncoder.encode(bytes), charset)

    override def decode(encodedText: String): Id[Array[Byte]] =
      java.util.Base64.getMimeDecoder.decode(encodedText)
  }
}
