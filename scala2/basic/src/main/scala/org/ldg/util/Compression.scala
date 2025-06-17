package org.ldg.util

/**
  * A tagless trait for compressing and decompressing byte arrays.
  * Usage method 1:
  * {{{
  *   import org.ldg.util.CompressionId.Implicits.compressGzipId
  *   import org.ldg.util.CompressionExt._
  *   val bytes: Array[Byte] = ???
  *
  *   val compressed = bytes.compressGzip
  *   val decompressed = compressed.decompressGzip
  * }}}
  * Usage method 2:
  * {{{
  *   import cats.Id
  *   import org.ldg.util.Compression
  *   import org.ldg.util.CompressionExt._
  *   import org.ldg.util.CompressionId.Implicits.compressGzipId
  *   val bytes: Array[Byte] = ???
  *
  *   val compressed = bytes.compress(Compression.Gzip[Id])
  *   val decompressed = compressed.decompress(Compression.Gzip[Id])
  * }}}
  * Usage method 3:
  * {{{
  *   import cats.Id
  *   import org.ldg.util.Compression
  *   import org.ldg.util.CompressionId.Implicits.compressGzipId
  *   val bytes: Array[Byte] = ???
  *
  *   val compressed = Compression.Gzip[Id].compress(bytes)
  *   val decompressed = Compression.Gzip[Id].decompress(compressed)
  * }}}
  * @tparam F effect type (e.g. Id, Future, IO, etc.)
  */
trait Compression[F[_]] {

  /**
    * Compresses the given byte array using an implementation defined compression method.
    * @param bytes the byte array to compress
    * @return a new byte array containing the compressed data
    */
  def compress( bytes: Array[Byte] ): F[Array[Byte]]

  /**
    * Decompresses the given byte array that was compressed.
    * @param bytes the byte array to decompress
    * @return a new byte array containing the decompressed data
    */
  def decompress( bytes: Array[Byte] ): F[Array[Byte]]
}

object Compression {

  /**
    * A class that delegates its implementation to another instance
    * Use to implement a Compression instance for Future, IO, etc. as just a wrapper of the Id implementation
    * @param delegate the delegate instance to use for encoding and decoding
    * @param deferG a function that defers the execution of the delegate methods to the effect type G
    * @tparam G the effect type to which the delegate methods will be deferred
    * @tparam F the effect type
    */
  class Delegate[G[_], F[_]]( delegate: Compression[F] )( implicit deferG: DeferredFunctionK[F, G] ) extends Compression[G] {
    override def compress( bytes: Array[Byte] ): G[Array[Byte]] =
      deferG( () => delegate.compress( bytes ) )
    override def decompress( bytes: Array[Byte] ): G[Array[Byte]] =
      deferG( () => delegate.decompress( bytes ) )
  }

  // supported compression protocols

  trait Gzip[F[_]] extends Compression[F]
  object Gzip {
    def apply[F[_]]( implicit gzip: Gzip[F] ): Gzip[F] = gzip
  }

  // maybe-do: add more compression protocols like deflate, Snappy, LZ4, etc.
  // maybe-do: add parallel GZip (only helpful for large file processing, web servers generally won't benefit from it)
  // see: https://news.ycombinator.com/item?id=33238283 & https://github.com/gregsh/parallel-zip
}
