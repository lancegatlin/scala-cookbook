package org.ldg.util

import cats.Id
import org.ldg.util.ByteArrayExt._
import org.ldg.util.CompressionExt._
import org.ldg.util.CompressionId.Implicits.compressGzipId
import org.ldg.util.StringExt.charset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompressionExtSpec extends AnyFlatSpec with Matchers {

  "compress/decompress" should "act as syntactic-sugar for Compress/Decompress" in {
    val input = "Hello, World!"
    val compressed = input.compress( Compression.Gzip[Id] )
    compressed should not be empty
    val decompressed = compressed.decompress( Compression.Gzip[Id] )
    decompressed.toStringFromBytes shouldEqual input
  }

  "compressGzip/decompressGzip" should "act as syntactic-sugar for Compress.Gzip/Decompress.Gzip" in {
    val input = "Hello, World!"
    val compressed = input.compressGzip
    compressed should not be empty
    val decompressed = compressed.decompressGzip
    decompressed.toStringFromBytes shouldEqual input
  }
}
