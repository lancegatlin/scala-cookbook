package org.ldg.util

import enumeratum._

sealed trait CompressionMethod extends EnumEntryExt {
  def toCompression[F[_]]( implicit compressionMethodResolver: CompressionMethodResolver[F] ): Compression[F] =
    compressionMethodResolver.resolve( this )
}

object CompressionMethod extends Enum[CompressionMethod] {
  // see https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Encoding
  // note: does not support multiple encodings and for now only gzip
  def parse( contentEncoding: String ): CompressionMethod =
    withNameInsensitiveOption( contentEncoding )
      .getOrElse( throw new IllegalArgumentException( s"Invalid CompressionMethod: $contentEncoding" ) )

  case object Gzip extends CompressionMethod

  override val values: IndexedSeq[CompressionMethod] = findValues
}
