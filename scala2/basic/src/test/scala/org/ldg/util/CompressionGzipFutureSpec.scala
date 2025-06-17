package org.ldg.http.util

import org.ldg.http.util.CompressionFuture.Implicits.gzipFuture
import org.ldg.util.{Compression, CompressionSpecBase}
import org.apache.pekko.actor.ActorSystem
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, ExecutionContext, Future}

class CompressionGzipFutureSpec extends CompressionSpecBase[Future] with ScalaFutures {
  implicit val system: ActorSystem = ActorSystem( "test" )
  implicit val executionContext: ExecutionContext = system.dispatcher

  override def unsafeRun[A]( fa: Future[A] ): A = Await.result( fa, implicitly[PatienceConfig].timeout )
  override def fixtures: Seq[Fixture] = Seq(
    Fixture(
      name = "Compression.Gzip[Future]",
      compression = Compression.Gzip[Future],
      maybeExpectedException = Some( new java.util.zip.ZipException )
    )
  )

  runTests()
}
