package org.ldg.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompressionUsageSpec extends AnyFlatSpec with Matchers {
  "Usage1" should "compile" in {
    object Usage1 {
      import org.ldg.util.CompressionId.Implicits.compressGzipId
      import org.ldg.util.CompressionExt._

      val bytes: Array[Byte] = ???

      val compressed = bytes.compressGzip
      val decompressed = compressed.decompressGzip
    }
  }

  "Usage2" should "compile" in {
    object Usage2 {
      import cats.Id
      import org.ldg.util.Compression
      import org.ldg.util.CompressionExt._
      import org.ldg.util.CompressionId.Implicits.compressGzipId
      val bytes: Array[Byte] = ???

      val compressed = bytes.compress( Compression.Gzip[Id] )
      val decompressed = compressed.decompress( Compression.Gzip[Id] )
    }
  }

  "Usage3" should "compile" in {
    object Usage3 {
      import cats.Id
      import org.ldg.util.Compression
      import org.ldg.util.CompressionId.Implicits.compressGzipId
      val bytes: Array[Byte] = ???

      val compressed = Compression.Gzip[Id].compress( bytes )
      val decompressed = Compression.Gzip[Id].decompress( compressed )
    }
  }
}
