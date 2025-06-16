package org.ldg.util

import cats.Id

import scala.concurrent.{ExecutionContext, Future}
import org.ldg.util.Base64Id.Implicits._

object Base64Future {
  object Implicits {
    // note: import only one of these implicits at a time, depending on the desired Base64 encoding scheme
    implicit def base64DefaultFuture(implicit executionContext: ExecutionContext): Base64[Future] =
      new Rfc4648
    implicit def base64Rfc4648Future(implicit executionContext: ExecutionContext): Base64.Rfc4648[Future] =
      new Rfc4648
    implicit def base64UrlSafeFuture(implicit executionContext: ExecutionContext): Base64.UrlSafe[Future] =
      new UrlSafe
    implicit def base64MimeFuture(implicit executionContext: ExecutionContext): Base64.Mime[Future] =
      new Mime
  }

  private implicit def idToFuture(implicit executionContext: ExecutionContext): DeferredFunctionK[Id, Future] =
    new DeferredFunctionK[Id, Future] {
      override def apply[A](fa: () => Id[A]): Future[A] = Future(fa())(executionContext)
    }

  class Rfc4648(implicit executionContext: ExecutionContext) extends
    ByteCoder.Delegate[Future, Id](Base64.Rfc4648[Id]) with
    Base64.Rfc4648[Future]

  class UrlSafe(implicit executionContext: ExecutionContext) extends
    ByteCoder.Delegate[Future, Id](Base64.UrlSafe[Id]) with
    Base64.UrlSafe[Future]

  class Mime(implicit executionContext: ExecutionContext) extends
    ByteCoder.Delegate[Future, Id](Base64.Mime[Id]) with
    Base64.Mime[Future]

  // maybe-do: add Par support if required (generally not needed for Base64 99% of encoding/decoding)
}
