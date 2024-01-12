package org.ldg
case class CaseInsensitiveString(value: String) {
  private lazy val cachedHashCode = value.toLowerCase.hashCode
  override def hashCode(): Int = cachedHashCode

  override def equals(obj: Any): Boolean =
    obj match {
      case s:String => value.equalsIgnoreCase(s)
      case _ => value.equals(obj)
    }

  override def toString: String = value
}

object CaseInsensitiveString {
  trait Implicits {
    import scala.language.implicitConversions

    implicit def stringToCaseInsensitiveString(s: String): CaseInsensitiveString =
      CaseInsensitiveString(s)

    implicit def caseInsensitiveStringToString(s: CaseInsensitiveString): String =
      s.value
  }
  object Implicits extends Implicits
}