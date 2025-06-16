package org.ldg.util

import java.nio.charset.Charset

object StringExt {
  val defaultCharset: Charset = Charset.defaultCharset()
  // note: using name `charset` to allow for easy implicit shadowing in methods (e.g. in toBytes below)
  implicit val charset: Charset = defaultCharset

  implicit class UtilStringExt(val self: String) extends AnyVal {
    def toBytes(implicit charset: Charset): Array[Byte] =
      self.getBytes(charset)
  }
}
