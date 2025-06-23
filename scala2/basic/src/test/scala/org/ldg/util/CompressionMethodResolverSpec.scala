package org.ldg.util

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompressionMethodResolverSpec extends AnyFlatSpec with Matchers {

  "CompressionMethodResolver[Id]" should "resolve Gzip to Compression.Gzip[Id]" in {
    val resolver = CompressionMethodResolver[Id]
    resolver.resolve(CompressionMethod.Gzip) shouldBe a[CompressionGzipId]
  }
}