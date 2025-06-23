package org.ldg.util

import cats.Id

/**
  * A tagless trait for converting a CompressionMethod to a Compression instance for a specific effect type.
  *
  * @tparam F the effect type (e.g. Id, Future, IO, etc.)
  */
trait CompressionMethodResolver[F[_]] {
  def resolve( compressionMethod: CompressionMethod ): Compression[F]
}

object CompressionMethodResolver {
  def apply[F[_]]( implicit resolver: CompressionMethodResolver[F] ): CompressionMethodResolver[F] = resolver

  // note: putting implicit for Id here means no import is required for CompressionMethodResolver[Id] instance
  // e.g., mystring.toCompressionMethod.toCompression will automatically find this implicit
  // CompressionMethodResolver[Id] instance without import or needing to specify Id
  implicit def compressionMethodResolverId(implicit gzipConfig: GzipConfig): CompressionMethodResolver[Id] = {
    case CompressionMethod.Gzip => new CompressionGzipId(gzipConfig)
  }
}
