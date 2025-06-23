package org.ldg.util

import cats.Id
import org.ldg.util.Compression.Gzip.compressionGzipId

class CompressionIdGzipSpec extends CompressionSpecBase[Id] {
  override def unsafeRun[A]( fa: Id[A] ): A = fa
  override def fixtures: Seq[Fixture] = Seq(
    Fixture(
      name = "Compression.Gzip[Id]",
      compression = Compression.Gzip[Id],
      maybeExpectedException = Some( new java.util.zip.ZipException )
    )
  )

  runTests()
}
