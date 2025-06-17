package org.ldg.util

/**
  * Configuration for GZIP compression and decompression. See java.util.zip.Deflater for details.
  * Note: implementations may ignore these settings
  * @param levelHint compression level
  * @param strategyHint compression strategy
  * @param bufferSizeHint size of the buffer to use for compression and decompression
  */
case class GzipConfig(
    levelHint: Int,
    strategyHint: Int,
    bufferSizeHint: Int
)

object GzipConfig {
  implicit val default: GzipConfig = GzipConfig(
    levelHint = java.util.zip.Deflater.DEFAULT_COMPRESSION,
    strategyHint = java.util.zip.Deflater.DEFAULT_STRATEGY,
    bufferSizeHint = 64 * 1024 //64k
  )
}
