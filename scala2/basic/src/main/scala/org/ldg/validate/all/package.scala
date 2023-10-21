package org.ldg.validate

import org.ldg.validate.{Validator => ValidatorAlias}

package object all extends StdValidators with BuiltInValidators {
  type Validator[A] = ValidatorAlias[A]

  implicit class ValidatorExt[A]( val self: Validator[A] ) extends AnyVal {

    /**
      * @return a Validator that accumulates errors from both Validators
      */
    def and( other: Validator[A] ): Validator[A] = Validator.and( self, other )
  }
}
