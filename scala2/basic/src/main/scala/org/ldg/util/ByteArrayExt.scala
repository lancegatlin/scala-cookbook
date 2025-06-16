package org.ldg.util

import java.nio.charset.{Charset, StandardCharsets}

object ByteArrayExt {

  implicit class UtilByteArrayExt(val self: Array[Byte]) extends AnyVal {
    def toStringFromBytes(implicit charset: Charset): String =
      new String(self, charset)

    def toUtf8String: String =
      new String(self, StandardCharsets.UTF_8)
  }

}
