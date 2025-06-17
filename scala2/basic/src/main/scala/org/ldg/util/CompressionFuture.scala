package org.ldg.http.util

import org.ldg.util.Compression
import org.apache.pekko.NotUsed
import org.apache.pekko.http.scaladsl.coding.Coders.Gzip
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.{Flow, Sink, Source}
import org.apache.pekko.util.ByteString

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

object CompressionFuture {
  object Implicits {
    implicit def gzipFuture(
        implicit
        materializer: Materializer,
        executionContext: ExecutionContext ): Compression.Gzip[Future] = new CompressionGzipFuture
  }

  private class CompressionGzipFuture(
      implicit
      materializer: Materializer,
      executionContext: ExecutionContext )
      extends Compression.Gzip[Future] {

    private def runFlow( bytes: Array[Byte] )( flow: Flow[ByteString, ByteString, NotUsed] ): Future[Array[Byte]] =
      Source
        .single( ByteString( immutable.ArraySeq.unsafeWrapArray( bytes ) ) )
        .via( flow )
        .runWith( Sink.fold( ByteString.empty )( _ ++ _ ) )
        .map( _.toArray )

    override def compress( bytes: Array[Byte] ): Future[Array[Byte]] =
      // note: Pekko Gzip doesn't expose compression level publicly (though it does support it internally)
      // its default is 6. It also doesn't expose strategy or buffer size at all.
      runFlow( bytes )( Gzip.encoderFlow )

    override def decompress( bytes: Array[Byte] ): Future[Array[Byte]] =
      runFlow( bytes )( Gzip.decoderFlow )
  }
}
