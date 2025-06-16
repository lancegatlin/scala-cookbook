package org.ldg.util

import java.nio.charset.Charset

object ByteCoderExt {

  implicit class ByteCoderStringExt(val self: Array[Byte]) extends AnyVal {
    def encode[F[_]](encoder: ByteCoder[F])(implicit charset: Charset): F[String] =
      encoder.encode(self)

    def encodeBase64[F[_]](implicit base64: Base64[F], charset: Charset): F[String] =
      base64.encode(self)
  }

  implicit class ByteCoderByteArrayExt(val self: String) extends AnyVal {
    def decode[F[_]](decoder: ByteCoder[F]): F[Array[Byte]] =
      decoder.decode(self)

    def decodeBase64[F[_]](implicit base64: Base64[F]): F[Array[Byte]] =
      base64.decode(self)
  }

}
