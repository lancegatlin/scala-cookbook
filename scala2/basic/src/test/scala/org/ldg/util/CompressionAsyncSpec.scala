package org.ldg.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.ldg.util.CompressionAsync.Implicits.compressionGzipAsync

import scala.concurrent.duration.DurationInt

class CompressionAsyncSpec extends CompressionSpecBase[IO] {
  override def unsafeRun[A]( fa: IO[A] ): A =
    fa.unsafeRunTimed( 200.millis ).getOrElse(
        throw new RuntimeException( "IO operation timed out" )
      )
  override def fixtures: Seq[Fixture] = Seq(
    Fixture(
      name = "Compression.Gzip[IO]",
      compression = Compression.Gzip[IO],
      maybeExpectedException = Some( new java.io.EOFException() )
    )
  )

  runTests()
}
