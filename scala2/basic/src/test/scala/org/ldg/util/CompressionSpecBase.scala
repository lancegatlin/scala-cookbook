package org.ldg.util

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.Charset
import scala.util.Random

abstract class CompressionSpecBase[F[_]] extends AnyFreeSpec with Matchers {

  def genTestSampleSize(): Int = Random.nextInt( 63 * 1024 ) + 1024 // between 1K and 64KB
  val defaultCharset: Charset = Charset.defaultCharset()
  def unsafeRun[A]( fa: F[A] ): A

  case class Fixture(
      name: String,
      compression: Compression[F],
      maybeExpectedException: Option[Throwable]
  ) {
    def testCompressDecompress( inputBytes: Array[Byte] ): Array[Byte] = {
      val compressedBytes = unsafeRun( compression.compress( inputBytes ) )
      compressedBytes.length should be > 0
      val outputBytes = unsafeRun( compression.decompress( compressedBytes ) )
      outputBytes.length shouldBe inputBytes.length
      // note: not using `shouldBe` here because it dumps the entire byte array on failure, which is not ideal for large arrays
      outputBytes.sameElements( inputBytes ) shouldBe true
      outputBytes
    }

    def testCompressDecompressString( input: String ): Unit = {
      val inputBytes = input.getBytes( defaultCharset )
      val outputBytes = testCompressDecompress( inputBytes )
      outputBytes shouldBe inputBytes
      new String( outputBytes, defaultCharset ) shouldBe input
    }
  }

  def fixtures: Seq[Fixture]

  def runTests(): Unit =
    fixtures.foreach { fixture =>
      import fixture._

      s"Compression - $name" - {

        "should compress/decompress string" in {
          testCompressDecompressString( "Hello, World!" )
        }

        val testSampleSize = genTestSampleSize()
        s"should compress/decompress GZIP $testSampleSize random bytes" in {
          testCompressDecompressString( Random.nextString( testSampleSize ) )
        }

        "should compress/decompress GZIP 1 byte" in {
          testCompressDecompress( Array( 123.toByte ) )
        }

        "should compress/decompress GZIP 0 bytes" in {
          testCompressDecompress( Array.empty )
        }

        "should throw an exception for invalid compressed input" in {
          maybeExpectedException match {
            case Some( expectedException ) =>
              val invalidInput = Array[Byte]( 1, 2, 3, 4 )
              val result = intercept[Throwable] {
                unsafeRun( compression.decompress( invalidInput ) )
              }
              result.getClass shouldBe expectedException.getClass
            case None => // do nothing
          }
        }
      }
    }
}
