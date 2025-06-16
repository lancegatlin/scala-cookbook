package org.ldg.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import Base64Async.Implicits._

import scala.concurrent.duration.DurationInt

class Base64AsyncSpec extends ByteCoderSpecBase[IO] {
  override def unsafeRun[A](fa: IO[A]): A = fa.unsafeRunTimed(150.millis).getOrElse(
    throw new RuntimeException("IO operation timed out")
  )

  def fixtures: Seq[Fixture] = Seq(
    Fixture("Base64[IO]", Base64[IO](base64DefaultAsync), Some(new IllegalArgumentException)),
    Fixture("Base64.Rfc4648[IO]", Base64.Rfc4648[IO], Some(new IllegalArgumentException)),
    Fixture("Base64.UrlSafe[IO]", Base64.UrlSafe[IO], Some(new IllegalArgumentException)),
    Fixture("Base64.Mime[IO]", Base64.Mime[IO], None)
  )
}