package org.ldg.util

import cats.Id

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip._
import scala.util.Using

class CompressionGzipId( gzipConfig: GzipConfig ) extends Compression.Gzip[Id] {
  override def compress( bytes: Array[Byte] ): Id[Array[Byte]] = {
    val byteStream = new ByteArrayOutputStream()
    // note: GZIPOutputStream does not support setting compression level or strategy
    // maybe-do: java.util.zip.Deflater could be used directly to allow setting level and strategy but would require
    // finding an open source implementation or writing our own GZIPOutputStream (which handles headers, checksums,
    // etc.)
    val out = new GZIPOutputStream( byteStream, gzipConfig.bufferSizeHint )
    Using.resource( out )( _.write( bytes ) )
    byteStream.toByteArray
  }

  override def decompress( bytes: Array[Byte] ): Id[Array[Byte]] = {
    val byteStream = new java.io.ByteArrayOutputStream()
    val in = new ByteArrayInputStream( bytes )
    Using.resource( new GZIPInputStream( in, gzipConfig.bufferSizeHint ) )( _.transferTo( byteStream ) )
    byteStream.toByteArray
  }
}
