package org.ldg.util

/**
  * A tagless trait for a Base64 encoding and decoding scheme
  * Usage method 1:
  * {{{
  *   import org.ldg.util.StringExt.charset
  *   import org.ldg.util.Base64Id.Implicits.base64DefaultId // select which base64 encoding scheme to use
  *   import org.ldg.util.ByteCoderExt._
  *   val bytes: Array[Byte] = ???
  *
  *   val encodedText = bytes.encodeBase64 // uses in scope implicit Base64[Id] instance (Rfc4648 by default)
  *   val decodedBytes = encodedText.decodeBase64
  * }}}
  * Usage method 2:
  * {{{
  *   import cats.Id
  *   import org.ldg.util.StringExt.charset
  *   import org.ldg.util.Base64Id.Implicits.base64UrlSafeId
  *   import org.ldg.util.ByteCoderExt._
  *   val bytes: Array[Byte] = ???
  *
  *   val encodedText2 = bytes.encode(Base64.UrlSafe[Id])
  *   val decodedBytes2 = encodedText2.decode(Base64.UrlSafe[Id])
  * }}}
  * Usage method 3:
  * {{{
  *   import cats.Id
  *   import org.ldg.util.StringExt.charset
  *   import org.ldg.util.Base64Id.Implicits.base64UrlSafeId
  *   val bytes: Array[Byte] = ???
  *
  *   val encodedText = Base64[Id].encode(bytes)
  *   val decodedBytes = Base64[Id].decode(encodedText)
  * }}}
  * @tparam F the effect type
  */
trait Base64[F[_]] extends ByteCoder[F]

object Base64 {
  def apply[F[_]]( implicit base64: Base64[F] ): Base64[F] = base64

  // supported encoding schemes

  trait Rfc4648[F[_]] extends Base64[F]
  object Rfc4648 {
    def apply[F[_]]( implicit rfc4648: Rfc4648[F] ): Rfc4648[F] = rfc4648
  }

  trait UrlSafe[F[_]] extends Base64[F]
  object UrlSafe {
    def apply[F[_]]( implicit urlSafe: UrlSafe[F] ): UrlSafe[F] = urlSafe
  }

  trait Mime[F[_]] extends Base64[F]
  object Mime {
    def apply[F[_]]( implicit mime: Mime[F] ): Mime[F] = mime
  }
}
