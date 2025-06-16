package org.ldg.util

import org.ldg.util.Base64Id.Implicits._
import cats.Id

class Base64IdSpec extends ByteCoderSpecBase[Id] {
  override def unsafeRun[A](fa: Id[A]): A = fa

  def fixtures: Seq[Fixture] = Seq(
    Fixture("Base64[Id]", Base64[Id](base64DefaultId), Some(new IllegalArgumentException)),
    Fixture("Base64.Rfc4648[Id]", Base64.Rfc4648[Id], Some(new IllegalArgumentException)),
    Fixture("Base64.UrlSafe[Id]", Base64.UrlSafe[Id], Some(new IllegalArgumentException)),
    Fixture("Base64.Mime[Id]", Base64.Mime[Id], None)
  )
}