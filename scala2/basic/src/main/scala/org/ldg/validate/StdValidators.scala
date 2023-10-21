package org.ldg.validate

import cats.data.ValidatedNel

trait StdValidators {

  /**
    * @return Valid is value is valid otherwise invalid of NonEmptyList of errors
    */
  def validate[A]( a: A )( implicit validator: Validator[A] ): ValidatedNel[String, A] = validator( a )

  /**
    * @return Valid if field value is valid otherwise invalid of NonEmptyList of errors, ensuring field name is prepended
    *         to error messages
    */
  def validateField[A]( fieldName: String, fieldValue: A )( implicit validator: Validator[A] ): ValidatedNel[String, A] =
    Validator.prependScopeToErrors( validator( fieldValue ), fieldName )

  /**
    * @return a Validator that is Valid if all its elements are valid (or it is empty) and accumulates errors for
    *         any invalid elements, ensuring error messages are scoped with element index
    */
  implicit def optionValidator[A]( implicit validator: Validator[A] ): Validator[Option[A]] = Validator.option

  /**
    * @return a Validator that is Valid only if all its elements are valid (or it is empty) and accumulates errors for
    *         any invalid elements, ensuring error messages are scoped with element index
    */
  implicit def seqValidator[A]( implicit validator: Validator[A] ): Validator[Seq[A]] = Validator.seq
}
