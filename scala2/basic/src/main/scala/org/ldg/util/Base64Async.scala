package org.ldg.util

import cats.Id
import cats.effect.Async
import Base64Id.Implicits._

object Base64Async {
  object Implicits {
    // note: import only one of these implicits at a time, depending on the desired Base64 encoding scheme
    implicit def base64DefaultAsync[F[_]: Async]: Base64[F] =
      new Rfc4648
    implicit def base64Rfc4648Async[F[_]: Async]: Base64.Rfc4648[F] =
      new Rfc4648
    implicit def base64UrlSafeAsync[F[_]: Async]: Base64.UrlSafe[F] =
      new UrlSafe
    implicit def base64MimeAsync[F[_]: Async]: Base64.Mime[F] =
      new Mime
  }

  implicit private def idToAsync[F[_]: Async]: DeferredFunctionK[Id, F] =
    new DeferredFunctionK[Id, F] {
      override def apply[A]( fa: () => Id[A] ): F[A] = Async[F].delay( fa() )
    }

  // note: reviewed source code for fs2.text.base64.encode/decode, and while there might be some minor performance
  // improvements they do not provide url-safe or mime variants, so instead delegating to Java Base64 (by way of
  // Base64Id) and wrapping in F.delay here (this can be revisited later if needed without impacting downstream code).
  // https://github.com/typelevel/fs2/blob/b27f4eda55933c5cbfa7d1bac93422be6472dd90/core/shared/src/main/scala/fs2/text.scala#L721

  class Rfc4648[F[_]: Async] extends ByteCoder.Delegate[F, Id]( Base64.Rfc4648[Id] ) with Base64.Rfc4648[F]

  class UrlSafe[F[_]: Async] extends ByteCoder.Delegate[F, Id]( Base64.UrlSafe[Id] ) with Base64.UrlSafe[F]

  class Mime[F[_]: Async] extends ByteCoder.Delegate[F, Id]( Base64.Mime[Id] ) with Base64.Mime[F]
}