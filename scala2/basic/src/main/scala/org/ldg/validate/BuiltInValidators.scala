package org.ldg.validate

import cats.data.Validated
import org.apache.commons.lang3.StringUtils


trait BuiltInValidators {
  val notBlankStringValidator: Validator[String] = { s: String =>
    Validated.condNel(
      !StringUtils.isBlank( s ),
      s,
      "must not be empty or blank"
    )
  }

  val disallowedStrings = Seq( "N/A", "null" ).map( _.toLowerCase )
  val notDisallowedStringValidator: Validator[String] = { s: String =>
    Validated.condNel(
      !disallowedStrings.contains( s.toLowerCase ),
      s,
      "must not be disallowed string"
    )
  }

  implicit val stringValidator: Validator[String] =
    Validator.and(notBlankStringValidator, notDisallowedStringValidator )
}
