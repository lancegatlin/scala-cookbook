package org

import java.time.LocalDate
import java.util.concurrent.ConcurrentHashMap
import java.util.function.{BiFunction, Function => JFunction}

package object ldg {
    // thread safe map
    type TSMap[A,B] = ConcurrentHashMap[A,B]

  implicit class ConcurrentHashMapExt[A,B](val self: TSMap[A,B]) extends AnyVal {
    def getOrCompute(key: A)(calc: () => B) : B =
      self.computeIfAbsent(key, new JFunction[A,B] {
        override def apply(t: A): B = calc()
      })

    def applyOrCompute(key: A)(calc: B => B) : B =
      self.computeIfPresent(key, new BiFunction[A,B,B] {
        override def apply(t: A, u: B): B = calc(u)
      })
  }

  implicit class CatUtilAnyExt[A]( val self: A ) extends AnyVal {

    /**
      * Run a side-effect on self and return self
      *
      * @param sideEffect side-effect function
      * @return self
      */
    def tap( sideEffect: A => Unit ): A = {
      sideEffect( self )
      self
    }

    /**
      * Transform self
      *
      * @param f transformation function
      * @return result of calling f(self)
      */
    def transform( f: A => A ): A =
      f( self )

    /**
      * Transform self if test value is true
      * @param test true to transform self
      * @param f transformation function
      * @return if test is TRUE then f(self) otherwise unmodified self
      */
    def transformIf( test: Boolean )( f: A => A ): A =
      if (test) f( self ) else self

    /**
      * Maybe transform self if opt is set otherwise return self
      *
      * @param ob optional value
      * @param f transformation function
      * @return f(value, self) or if ob unset unmodified self
      */
    def maybeTransform[B]( ob: Option[B] )( f: ( A, B ) => A ): A =
      ob.fold( self )( b => f( self, b ) )

    /**
      * Consecutively transform self for each value in a collection
      *
      * @param xb collection of values
      * @param f transformation function
      * @return new self
      */
    def foldTransform[B]( xb: IterableOnce[B] )( f: ( A, B ) => A ): A =
      xb.iterator.foldLeft[A]( self )( f )
  }

  object TryIt {
    val builder = List.newBuilder[Int]
    builder.tap(_ += 1).tap(_ += 2).tap(_ ++= Seq(3,4,5))

    builder.transform(_ += 1).transform(_ += 2).transform(_ ++= Seq(3,4,5))

    val maybeV = Option(123456)
    builder.maybeTransform(maybeV)(_ += _)

    builder.foldTransform(Seq(11,12,13))(_ += _)
    builder.foldTransform(maybeV)(_ += _)
  }

  implicit class MapStringStringExt(val self: Map[String,String]) extends AnyVal {
    def toProperties : java.util.Properties = {
      val retv = new java.util.Properties
      self.foreach { case (k,v) => retv.setProperty(k,v) }
      retv
    }
  }

  /**
    * Used to end job execution when a fatal error occurs with a message
    * @param msg message explaining job failure
    */
  def die(msg: String) = throw new RuntimeException(msg)

  implicit class OptionExt[A](val self: Option[A]) extends AnyVal {
    /**
      * Get the value inside an option or end job with a message
      * @param msg if no value, message to end job with
      * @return value
      */
    def getOrDie(msg: String) : A =
      self.getOrElse(die(msg))
  }

  implicit class EitherStringExt[A](val self: Either[String,A]) extends AnyVal {
    /**
      * Get the right value inside an Either or end job with a message
      * @param msg if no value, message to end job with
      * @return value
      */
    def getOrDie(msg: String) : A =
      self.fold(innerMsg => die(s"$msg: $innerMsg"), identity)
  }

  implicit class MapExt[A,B](val self: Map[A,B]) extends AnyVal {
    /**
      * Get the value for a key or end job with a message
      * @param msg if no value, message to end job with
      * @return value
      */
    def getOrDie(key: A, msg: String) : B =
      self.getOrElse(key, die(msg))
  }

  implicit class LocalDateExt(val self: LocalDate) extends AnyVal {
    def toSqlDate : java.sql.Date =
      java.sql.Date.valueOf(self)
  }
}
