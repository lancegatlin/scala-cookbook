package org.ldg.util

import cats.effect.{Async, Concurrent}
import org.ldg.util.Compression.Gzip
import de.lhns.fs2.compress.{GzipCompressor, GzipDecompressor}
import fs2.compression.{Compression => Fs2Compression}
import fs2.{Pipe, Stream}

object CompressionAsync {
  object Implicits {
    implicit def compressionGzipAsync[F[_]: Async: Fs2Compression]: Gzip[F] =
      new CompressionGzipAsync
  }

  private class CompressionGzipAsync[F[_]: Async: Concurrent: Fs2Compression](
      implicit
      gzipConfig: GzipConfig )
      extends Compression.Gzip[F] {
    private val compressor = GzipCompressor.make(
      deflateLevel = Some( gzipConfig.levelHint ),
      deflateStrategy = Some( gzipConfig.strategyHint ),
      chunkSize = gzipConfig.bufferSizeHint
    )
    private val decompressor = GzipDecompressor.make( gzipConfig.bufferSizeHint )

    private def runPipe( bytes: Array[Byte] )( pipe: Pipe[F, Byte, Byte] ): F[Array[Byte]] =
      Stream
        .emits( bytes )
        .lift[F]
        .through( pipe )
        .compile
        .to( Array )

    override def compress( bytes: Array[Byte] ): F[Array[Byte]] =
      runPipe( bytes )( compressor.compress )

    override def decompress( bytes: Array[Byte] ): F[Array[Byte]] =
      runPipe( bytes )( decompressor.decompress )
  }
}
