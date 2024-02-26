package org.ldg.util

object AnyTapExt {
  implicit class OrgLdgUtilAnyTapExt[A]( val self: A ) extends AnyVal {

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
     * Call a side-effect function if test value is TRUE
     *
     * @param test if TRUE call side-effect function
     * @param sideEffect side-effect function
     * @return self
     */
    def tapIf( test: Boolean )( sideEffect: A => Unit ): A = {
      if (test) {
        sideEffect( self )
      }
      self
    }

    /**
     * Call a side-effect function if option is Some
     *
     * @param ob if Some call side-effect function
     * @param sideEffect side-effect function
     * @return self
     */
    def tapIf[B]( ob: Option[B] )( sideEffect: ( A, B ) => Unit ): A =
      ob.fold( self ) { b =>
        sideEffect( self, b )
        self
      }

    /**
     * Call a side-effect function for each value in a collection
     *
     * @param xb collection of values which will be used to call side-effect
     * @param sideEffect side-effect function
     * @return self
     */
    def tapForeach[B]( xb: IterableOnce[B] )( sideEffect: ( A, B ) => Unit ): A = {
      xb.iterator.foreach { b =>
        sideEffect( self, b )
      }
      self
    }
  }
}
