package org.ldg

import cats.effect.kernel.Clock
import cats.{Applicative, Id}

import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

package object effect {
  implicit object ClockForId extends Clock[Id] {
    override val applicative: Applicative[Id] = implicitly
    override def monotonic: Id[FiniteDuration] = System.nanoTime().nanos
    override def realTime: Id[FiniteDuration] = System.currentTimeMillis().millis
  }

  implicit def clockForFuture( implicit ec: ExecutionContext ): Clock[Future] = new Clock[Future] {
    override val applicative: Applicative[Future] = implicitly
    override def monotonic: Future[FiniteDuration] = Future.successful( System.nanoTime().nanos )
    override def realTime: Future[FiniteDuration] = Future.successful( System.currentTimeMillis().millis )
  }
}
