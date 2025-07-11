package org.ldg.util

import cats.Id
import org.ldg.util.ByteCoderExt._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class CompressionExtSpec extends AnyFlatSpec with Matchers {
  import org.ldg.util.CompressionExt._
  import org.ldg.util.Compression.Gzip.compressionGzipId

  def genInputString(): String = Random.nextString( Random.nextInt( 1024 ) + 128 )
  // todo: stringToBytes
  def genInputBytes(): Array[Byte] = genInputString().getBytes()

  "ByteArray.compress/decompress" should "act as syntactic-sugar for compress/decompress" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compress( Compression.Gzip[Id] )
    compressed should not be empty
    val decompressed = compressed.decompress( Compression.Gzip[Id] )
    decompressed shouldEqual inputBytes
  }

  "ByteArray.compressGzip/decompressGzip" should "act as syntactic-sugar for compressGzip/decompressGzip" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressGzip
    compressed should not be empty
    val decompressed = compressed.decompressGzip
    decompressed shouldEqual inputBytes
  }
  
  "ByteArray.compressIf" should "compress/decompress when enabled" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressIf( shouldCompress = true )( Compression.Gzip[Id] )
    compressed should not be empty
    compressed should not be inputBytes
    val decompressed = compressed.decompressIf( shouldDecompress = true )( Compression.Gzip[Id] )
    decompressed shouldEqual inputBytes
  }

  it should "not compress/decompress when disabled" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressIf( shouldCompress = false )( Compression.Gzip[Id] )
    compressed should not be empty
    compressed shouldBe inputBytes
    val decompressed = compressed.decompressIf( shouldDecompress = false )( Compression.Gzip[Id] )
    decompressed shouldEqual inputBytes
  }

  "ByteArray.compressIfSet" should "compress/decompress when set" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressIfSet( Some(Compression.Gzip))
    compressed should not be empty
    compressed should not be inputBytes
    val decompressed = compressed.decompressIf( shouldDecompress = true )( Compression.Gzip[Id] )
    decompressed shouldEqual inputBytes
  }

  it should "not compress/decompress when not set" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressIfSet[Id]( None )
    compressed should not be empty
    compressed shouldBe inputBytes
    val decompressed = compressed.decompressIf( shouldDecompress = false )( Compression.Gzip[Id] )
    decompressed shouldEqual inputBytes
  }

  "ByteArray.compressGzipIf" should "compressGzip/decompressGzip when enabled" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressGzipIf( shouldCompress = true )
    compressed should not be empty
    compressed should not be inputBytes
    val decompressed = compressed.decompressGzipIf( shouldDecompress = true )
    decompressed shouldEqual inputBytes
  }

  it should "not compressGzip/decompressGzip when disabled" in {
    val inputBytes = genInputBytes()
    val compressed = inputBytes.compressGzipIf( shouldCompress = false )
    compressed should not be empty
    compressed shouldBe inputBytes
    val decompressed = compressed.decompressGzipIf( shouldDecompress = false )
    decompressed shouldEqual inputBytes
  }
}
