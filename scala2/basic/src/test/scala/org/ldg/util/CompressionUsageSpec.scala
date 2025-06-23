package org.ldg.util

import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CompressionUsageSpec extends AnyFlatSpec with Matchers {
  "Usage1" should "compile" in {
    object Usage1 {
      import org.ldg.util.CompressionExt._
      val bytes: Array[Byte] = ???

      val compressed = bytes.compressGzip
      val decompressed = compressed.decompressGzip
    }
  }

  "Usage2" should "compile" in {
    object Usage2 {
      // can't actually import compressGzipFuture here since its in another package
      implicit val fakeCompressionGzipFuture: Compression.Gzip[Future] = ???

      import org.ldg.util.Compression
      import org.ldg.util.CompressionExt._
      //import org.ldg.util.CompressionFuture.Implicits.compressionGzipFuture
      val bytes: Array[Byte] = ???

      val fCompressed: Future[Array[Byte]] = bytes.compress( Compression.Gzip[Future] )
      val decompressed: Future[Array[Byte]] = fCompressed.flatMap( compressed => compressed.decompress( Compression.Gzip[Future] ) )
    }
  }

  "Usage3" should "compile" in {
    object Usage3 {
      // can't actually import compressGzipAsync here since its in another package
      implicit val fakeCompressionGzipIO: Compression.Gzip[IO] = ???

      import org.ldg.util.Compression
      //import org.ldg.util.CompressionAsync.Implicits.compressGzipAsync
      val bytes: Array[Byte] = ???

      val fCompressed: IO[Array[Byte]] = Compression.Gzip[IO].compress( bytes )
      val decompressed: IO[Array[Byte]] = fCompressed.flatMap( compressed => Compression.Gzip[IO].decompress( compressed ) )
    }
  }
}
