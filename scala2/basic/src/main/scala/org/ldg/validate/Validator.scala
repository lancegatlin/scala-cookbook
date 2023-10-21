package org.ldg.validate

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.catsSyntaxValidatedId

object Validator {
  def apply[A]( test: A => Boolean, e: String ): Validator[A] = { a =>
    Validated.condNel( test( a ), a, e )
  }

  /**
    * @return a Validator[Option [A] ] that is Valid for None or if Some value if the value is valid
    */
  def option[A]( implicit validator: Validator[A] ): Validator[Option[A]] = { oa: Option[A] =>
    oa.fold[ValidatedNel[String, Option[A]]]( None.valid )( a => validator( a ).map( Some.apply ) )
  }

  /**
    * @return a Validator that is Valid if all its elements are valid (or it is empty) and accumulates errors for
    *         any invalid elements, ensuring element index scope is prepended to error messages
    */
  def seq[A]( implicit validator: Validator[A] ): Validator[Seq[A]] = { xa =>
    val validated = xa.zipWithIndex.map { case ( a, index ) => ( validator( a ), index ) }
    val errors = validated.flatMap {
      case ( Invalid( errors ), index ) =>
        errors.map( prependScopeToError( s"[$index]", _ ) ).toList
      case _ => Nil
    }
    errors match {
      case Nil          => validated.collect { case ( Valid( a ), _ ) => a }.validNel
      case head :: tail => NonEmptyList( head, tail ).invalid
    }
  }

  /**
    * @return a Validator that is Valid if both return Valid otherwise accumulates errors from both Validators
    */
  def and[A]( lhs: Validator[A], rhs: Validator[A] ): Validator[A] = { a =>
    ( lhs( a ), rhs( a ) ) match {
      case ( valid @ Valid( _ ), Valid( _ ) )         => valid
      case ( Valid( _ ), invalid @ Invalid( _ ) )     => invalid
      case ( invalid @ Invalid( _ ), Valid( _ ) )     => invalid
      case ( Invalid( errors1 ), Invalid( errors2 ) ) => Invalid( errors1 ::: errors2 )
    }
  }

  /**
    * @return TRUE if error message is scoped already (e.g. "value: some error message") OR
    *         FALSE if not scoped (e.g. "some error message")
    */
  def errorIsScoped( error: String ): Boolean =
    error.find {
      case ' ' | ':' | '[' | '.' => true
      case _                     => false
    } match {
      case Some( ' ' ) => false
      case Some( _ )   => true
      case _           => false
    }

  /**
    * @return the error message with the scope string prepended (taking into account if error message already has scope)
    */
  def prependScopeToError( scope: String, error: String ): String =
    if (errorIsScoped( error )) {
      if (error.startsWith( "[" )) {
        s"$scope$error"
      } else {
        s"$scope.$error"
      }
    } else {
      s"$scope: $error"
    }

  def prependScopeToErrors[A]( v: ValidatedNel[String, A], scope: String ): ValidatedNel[String, A] =
    v.leftMap( _.map( prependScopeToError( scope, _ ) ) )
}
