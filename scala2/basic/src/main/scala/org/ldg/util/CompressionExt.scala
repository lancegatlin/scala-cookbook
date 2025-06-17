package org.ldg.util

import org.ldg.util.StringExt.UtilStringExt

import java.nio.charset.Charset

object CompressionExt {

  implicit class CompressionStringExt( val self: String ) extends AnyVal {
    def compress[F[_]]( compression: Compression[F] )( implicit charset: Charset ): F[Array[Byte]] =
      compression.compress( self.toBytes( charset ) )

    def compressGzip[F[_]]( implicit charset: Charset, compression: Compression.Gzip[F] ): F[Array[Byte]] =
      compression.compress( self.toBytes( charset ) )
  }

  implicit class CompressionByteArrayExt( val self: Array[Byte] ) extends AnyVal {
    def compress[F[_]]( compression: Compression[F] ): F[Array[Byte]] =
      compression.compress( self )

    def decompress[F[_]]( compression: Compression[F] ): F[Array[Byte]] =
      compression.decompress( self )

    def compressGzip[F[_]]( implicit compression: Compression.Gzip[F] ): F[Array[Byte]] =
      compression.compress( self )

    def decompressGzip[F[_]]( implicit compress: Compression.Gzip[F] ): F[Array[Byte]] =
      compress.decompress( self )
  }

}
