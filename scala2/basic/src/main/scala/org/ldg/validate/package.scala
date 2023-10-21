package org.ldg

import cats.data.ValidatedNel

package object validate {

  /** A typeclass for validating values */
  type Validator[A] = A => ValidatedNel[String, A]
}
