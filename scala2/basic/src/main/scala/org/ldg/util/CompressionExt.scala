package org.ldg.util

import cats.Monad
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps}
import org.ldg.util.AnyTransformExt.CatDigitalUtilAnyTransformExt

object CompressionExt {

  implicit class CompressionStringExt( val self: String ) extends AnyVal {

    /**
      * Parses the string to a CompressionMethod
      * @return the CompressionMethod parsed from the string OR throws IllegalArgumentException if the string is not a
      *         valid CompressionMethod
      */
    def toCompressionMethod: CompressionMethod =
      CompressionMethod.parse( self )
  }

  implicit class CompressionByteArrayExt( val self: Array[Byte] ) extends AnyVal {

    // compress

    /**
      * Compresses the byte array using the provided compression method.
      * @param compression the compression method to use
      * @tparam F the effect type
      * @return a compressed byte array wrapped in the effect type
      */
    def compress[F[_]]( compression: Compression[F] ): F[Array[Byte]] =
      compression.compress( self )

    /**
      * Maybe compress the byte array using Gzip compression.
      * @param shouldCompress if true, compress the byte array using Gzip, otherwise return the original byte array
      * @param compression the compression method to use
      * @tparam F the effect type
      * @return a Gzip compressed byte array if `shouldCompress` is true, otherwise the original byte array
      */
    def compressIf[F[_]: Monad]( shouldCompress: Boolean )( compression: Compression[F] ): F[Array[Byte]] =
      self.pure[F].transformIf( shouldCompress )( _.flatMap( _.compress( compression ) ) )

    /**
      * Maybe compress the byte array using the provided compression if set.
      * @param maybeCompression an optional compression method to use, if provided
      * @tparam F the effect type
      * @return a Gzip compressed byte array if `maybeCompression` is set, otherwise the original byte array
      */
    def compressIfSet[F[_]: Monad](
        maybeCompression: Option[Compression[F]]
    ): F[Array[Byte]] =
      self.pure[F].transformIf( maybeCompression )( ( fBytes, compression ) => fBytes.flatMap( _.compress( compression ) ) )

    /**
      * Compresses the byte array using Gzip compression.
      * @return a Gzip compressed byte array
      */
    def compressGzip: Array[Byte] =
      Compression.Gzip.compress( self )

    /**
      * Maybe compress the byte array using Gzip compression if `shouldCompress` is true.
      * @param shouldCompress if true, compress the byte array using Gzip, otherwise return the original byte array
      * @return a Gzip compressed byte array if `shouldCompress` is true, otherwise the original byte array
      */
    def compressGzipIf( shouldCompress: Boolean ): Array[Byte] =
      self.transformIf( shouldCompress )( _.compressGzip )

    // decompress

    /**
      * Decompresses the byte array using the provided compression method.
      * @param compression the compression method to use
      * @tparam F the effect type
      * @return a decompressed byte array wrapped in the effect type
      */
    def decompress[F[_]]( compression: Compression[F] ): F[Array[Byte]] =
      compression.decompress( self )

    /**
      * Maybe decompress the byte array using the provided compression method if `shouldDecompress` is true.
      * @param shouldDecompress if true, decompress the byte array using the provided compression method, otherwise
      *                         return the original byte array
      * @param compression the compression method to use
      * @tparam F the effect type
      * @return a decompressed byte array if `shouldDecompress` is true, otherwise the original byte array
      */
    def decompressIf[F[_]: Monad]( shouldDecompress: Boolean )( compression: Compression[F] ): F[Array[Byte]] =
      self.pure[F].transformIf( shouldDecompress )( _.flatMap( _.decompress( compression ) ) )

    /**
      * Maybe compress the byte array using the provided compression if set.
      * @param maybeCompression an optional compression method to use, if provided
      * @tparam F the effect type
      * @return a Gzip compressed byte array if `maybeCompression` is set, otherwise the original byte array
      */
    def decompressIfSet[F[_]: Monad](
        maybeCompression: Option[Compression[F]]
    ): F[Array[Byte]] =
      self.pure[F].transformIf( maybeCompression )( ( fBytes, compression ) => fBytes.flatMap( _.decompress( compression ) ) )

    /**
      * Decompresses the byte array using Gzip compression.
      * @return a decompressed byte array
      */
    def decompressGzip: Array[Byte] =
      Compression.Gzip.decompress( self )

    /**
      * Maybe decompress the byte array using Gzip compression if `shouldDecompress` is true.
      * @param shouldDecompress if true, decompress the byte array using Gzip, otherwise return the original byte array
      * @return a decompressed byte array if `shouldDecompress` is true, otherwise the original byte array
      */
    def decompressGzipIf( shouldDecompress: Boolean ): Array[Byte] =
      self.transformIf( shouldDecompress )( _.decompressGzip )

  }

}
