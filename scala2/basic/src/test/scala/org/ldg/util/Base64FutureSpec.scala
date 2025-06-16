package org.ldg.util

import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits._
import Base64Future.Implicits._

class Base64FutureSpec extends ByteCoderSpecBase[Future] with ScalaFutures {
  override def unsafeRun[A](fa: Future[A]): A = Await.result(fa, implicitly[PatienceConfig].timeout)

  def fixtures: Seq[Fixture] = Seq(
    Fixture("Base64[Future]", Base64[Future](base64DefaultFuture), Some(new IllegalArgumentException)),
    Fixture("Base64.Rfc4648[Future]", Base64.Rfc4648[Future], Some(new IllegalArgumentException)),
    Fixture("Base64.UrlSafe[Future]", Base64.UrlSafe[Future], Some(new IllegalArgumentException)),
    Fixture("Base64.Mime[Future]", Base64.Mime[Future], None)
  )
}