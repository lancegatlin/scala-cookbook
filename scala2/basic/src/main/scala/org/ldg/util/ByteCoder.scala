package org.ldg.util

import java.nio.charset.Charset


/**
 * A tagless trait for encoding and decoding bytes to and from text
 * Usage:
 * {{{
 *   import org.ldg.util.ByteCoder
 *   // select which ByteCoder instance to use
 *   import org.ldg.util.Base64Id.Implicits.base64DefaultId
 *   import org.ldg.util.ByteCoderExt._
 *
 *   val bytes: Array[Byte] = ???
 *   val encodedText = bytes.encode(Base64[Id]) // uses in scope implicit Base64[Id] instance (Rfc4648 by default)
 *   val decodedBytes = encodedText.decode(Base64[Id]) // uses in scope implicit Base64[Id] instance (Rfc4648 by default)
 * }}}
 * @tparam F the effect type
 */
trait ByteCoder[F[_]] {
  /**
   * Encodes the given byte array to an encoded string (e.g. base64 encoded)
   * @param bytes the byte array to encode
   * @param charset the charset to use for encoding the string. Use StandardCharsets UTF-8, ISO_8859_1 or US_ASCII to
   *                avoid String re-encoding.
   * @return an encoded string representation of the byte array
   */
  def encode(bytes: Array[Byte])(implicit charset: Charset): F[String]

  /**
   * Decode an encoded string back to a byte array.
   * @param encodedText the encoded string to decode
   * @return the byte array decoded from the encoded string
   */
  def decode(encodedText: String): F[Array[Byte]]
}

object ByteCoder {
  /**
   * A class that delegates its implementation to another instance
   * Use to implement a ByteCoder instance for Future, IO, etc. as just a wrapper of the Id implementation
   * @param delegate the delegate instance to use for encoding and decoding
   * @param deferG a function that defers the execution of the delegate methods to the effect type G
   * @tparam G the effect type to which the delegate methods will be deferred
   * @tparam F the effect type
   */
  class Delegate[G[_], F[_]](delegate: ByteCoder[F])(implicit deferG: DeferredFunctionK[F, G]) extends ByteCoder[G] {
    override def encode(bytes: Array[Byte])(implicit charset: Charset): G[String] =
      deferG(() => delegate.encode(bytes)(charset))
    override def decode(encodedText: String): G[Array[Byte]] =
      deferG(() => delegate.decode(encodedText))
  }
}
