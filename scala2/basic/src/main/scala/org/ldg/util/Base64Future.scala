package org.ldg.util

import cats.Id
import Base64Id.Implicits._

import scala.concurrent.{ExecutionContext, Future}

object Base64Future {
  object Implicits {
    // note: import only one of these implicits at a time, depending on the desired Base64 encoding scheme
    implicit def base64DefaultFuture( implicit executionContext: ExecutionContext ): Base64[Future] =
      new Rfc4648
    implicit def base64Rfc4648Future( implicit executionContext: ExecutionContext ): Base64.Rfc4648[Future] =
      new Rfc4648
    implicit def base64UrlSafeFuture( implicit executionContext: ExecutionContext ): Base64.UrlSafe[Future] =
      new UrlSafe
    implicit def base64MimeFuture( implicit executionContext: ExecutionContext ): Base64.Mime[Future] =
      new Mime
  }

  implicit private def idToFuture( implicit executionContext: ExecutionContext ): DeferredFunctionK[Id, Future] =
    new DeferredFunctionK[Id, Future] {
      override def apply[A]( fa: () => Id[A] ): Future[A] = Future( fa() )( executionContext )
    }

  // note: reviewed Pekko official ByteString(bytes).encodeBase64/decodeBase64 source code, and they are simply calling
  // Java Base64, so just delegating to Java Base64 (by way of Base64Id) and wrapping with Future.apply here (this can
  // be revisited later if needed without impacting downstream code).
  // https://github.com/akka/akka/blob/0bbe6d5ba01adf0ef060e0afaae4ebf42ee670cd/akka-actor/src/main/scala-2.13/akka/util/ByteString.scala#L206

  class Rfc4648( implicit executionContext: ExecutionContext ) extends ByteCoder.Delegate[Future, Id]( Base64.Rfc4648[Id] ) with Base64.Rfc4648[Future]

  class UrlSafe( implicit executionContext: ExecutionContext ) extends ByteCoder.Delegate[Future, Id]( Base64.UrlSafe[Id] ) with Base64.UrlSafe[Future]

  class Mime( implicit executionContext: ExecutionContext ) extends ByteCoder.Delegate[Future, Id]( Base64.Mime[Id] ) with Base64.Mime[Future]
}