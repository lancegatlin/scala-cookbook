package org.ldg.util

object AnyTransformExt {
  implicit class OrgLdgUtilAnyTransformExt[A]( val self: A ) extends AnyVal {

    /**
      * Transform self
      *
      * @param f transformation function
      * @return result of calling f(self)
      */
    def transform[B]( f: A => B ): B =
      f( self )

    /**
      * Transform self if test value is TRUE
      *
      * @param test TRUE to transform self
      * @param f transformation function
      * @return if test is TRUE then f(self) otherwise unmodified self
      */
    def transformIf( test: Boolean )( f: A => A ): A =
      if (test) f( self ) else self

    /**
      * Transform self if option is Some
      *
      * @param ob optional value
      * @param f transformation function
      * @return f(value, self) or if ob unset unmodified self
      */
    def transformIf[B]( ob: Option[B] )( f: ( A, B ) => A ): A =
      ob.fold( self )( b => f( self, b ) )

    /**
      * Consecutively transform self for each value, left to right, in a collection
      *
      * @param xb collection of values
      * @param f transformation function
      * @return new self
      */
    def transformFold[B]( xb: IterableOnce[B] )( f: ( A, B ) => A ): A =
      xb.iterator.foldLeft[A]( self )( f )
  }
}
