package org.ldg.util

import cats.Id
import cats.effect.Async
import org.ldg.util.Base64Id.Implicits._

object Base64Async {
  object Implicits {
    // note: import only one of these implicits at a time, depending on the desired Base64 encoding scheme
    implicit def base64DefaultAsync[F[_]:Async]: Base64[F] =
      new Rfc4648
    implicit def base64Rfc4648Async[F[_]:Async]: Base64.Rfc4648[F] =
      new Rfc4648
    implicit def base64UrlSafeAsync[F[_]:Async]: Base64.UrlSafe[F] =
      new UrlSafe
    implicit def base64MimeAsync[F[_]:Async]: Base64.Mime[F] =
      new Mime
  }

  private implicit def idToAsync[F[_]:Async]: DeferredFunctionK[Id, F] =
    new DeferredFunctionK[Id, F] {
      override def apply[A](fa: () => Id[A]): F[A] = Async[F].delay(fa())
    }

  class Rfc4648[F[_]:Async] extends
    ByteCoder.Delegate[F, Id](Base64.Rfc4648[Id]) with
    Base64.Rfc4648[F]

  class UrlSafe[F[_]:Async] extends
    ByteCoder.Delegate[F, Id](Base64.UrlSafe[Id]) with
    Base64.UrlSafe[F]

  class Mime[F[_]:Async] extends
    ByteCoder.Delegate[F, Id](Base64.Mime[Id]) with
    Base64.Mime[F]
    
  // maybe-do: add Par support if required (generally not needed for Base64 99% of encoding/decoding)
}
