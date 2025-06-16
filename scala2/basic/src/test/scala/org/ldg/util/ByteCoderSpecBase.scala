package org.ldg.util

import org.ldg.util.StringExt._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

abstract class ByteCoderSpecBase[F[_]] extends AnyFreeSpec with Matchers {

  def unsafeRun[A](fa: F[A]): A

  case class Fixture(
    name: String,
    coder: ByteCoder[F],
    maybeExpectedException: Option[Throwable]
  ) {
    def testEncodeDecode(inputBytes: Array[Byte]): Array[Byte] = {
      val encodedText: String = unsafeRun(coder.encode(inputBytes))
      if(inputBytes.length > 0) {
        encodedText.length should be > 0
      }
      val decodedBytes = unsafeRun(coder.decode(encodedText))
      decodedBytes shouldBe inputBytes
      decodedBytes
    }

    def testEncodeDecode(input: String): Unit = {
      val inputBytes = input.getBytes
      val outputBytes = testEncodeDecode(inputBytes)
      outputBytes shouldBe inputBytes
      new String(outputBytes, defaultCharset) shouldBe input
    }

    def testEncodeDecode(testTextSize: Int): Unit =
      testEncodeDecode(Random.nextBytes(testTextSize))
  }

  def fixtures: Seq[Fixture]

  def genTestSampleSize: Int = Random.nextInt(64 * 1024) + 1 // between 1 and 64KB


  fixtures.foreach { fixture =>
    import fixture._

    s"ByteCoder - $name" - {
      "should encode/decode bytes" in {
        testEncodeDecode(genTestSampleSize)
      }

      "should encode/decode 1 byte" in {
        testEncodeDecode(1)
      }

      "should encode/decode 0 byte" in {
        testEncodeDecode(0)
      }

      "should throw an exception for invalid encoded text" in {
        maybeExpectedException match {
          case Some(expectedException) =>
            val invalidBase64Text = "㕪Ⴂ鯗ᣮ蒀㕭們刌叡"
            val result = intercept[Throwable] {
              unsafeRun(coder.decode(invalidBase64Text))
            }
            result.getClass shouldBe expectedException.getClass
          case None => // do nothing
        }
      }

    }
  }
}
