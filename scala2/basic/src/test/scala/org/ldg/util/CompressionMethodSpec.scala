package org.ldg.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompressionMethodSpec extends AnyFlatSpec with Matchers {

  "parse" should "return Gzip for 'gzip' (case-insensitive)" in {
    CompressionMethod.parse("gzip") shouldBe CompressionMethod.Gzip
    CompressionMethod.parse("GZIP") shouldBe CompressionMethod.Gzip
    CompressionMethod.parse("GzIp") shouldBe CompressionMethod.Gzip
  }

  it should "throw IllegalArgumentException for unknown encoding" in {
    an [IllegalArgumentException] should be thrownBy CompressionMethod.parse("deflate")
    an [IllegalArgumentException] should be thrownBy CompressionMethod.parse("snappy")
    an [IllegalArgumentException] should be thrownBy CompressionMethod.parse("")
  }

  "toCompression" should "resolve to a Compression instance for supported effect types" in {
    val gzipId = CompressionMethod.Gzip.toCompression
    gzipId shouldBe a [CompressionGzipId]
  }
}
