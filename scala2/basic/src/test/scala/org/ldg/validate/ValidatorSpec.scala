
package org.ldg.validate

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ldg.validate.all._

class ValidatorSpec extends AnyFlatSpec with Matchers {
  case class TestInner( value: Double )
  object TestInner {
    implicit val testInnerValidator: Validator[TestInner] = { testInner =>
      validateField( "value", testInner.value )(
        Validator( _ >= 1000.0, "must be greater than or equal to 1000" )
      ).map( TestInner.apply )
    }
  }

  case class TestOuter(
      value1: Int,
      value2: String,
      inner: TestInner,
      seq: Seq[Int],
      option: Option[String],
      value3: String,
      nestedSeq: Seq[TestInner]
  )

  implicit val testOuterValidator: Validator[TestOuter] = { testOuter =>
    import testOuter._

    (
      validateField( "value1", value1 )(
        Validator( _ < 5, "must be less than 5" )
      ),
      validateField( "value2", value2 ),
      validateField( "inner", inner ),
      validateField( "seq", seq )(
        Validator[Seq[Int]]( _.size % 2 == 0, "size must be multiple of 2" )
          .and( Validator( _.size > 3, "size must be greater than 3" ) ).and(
            Validator.seq( Validator( v => v == 1 || v == 2, "must be 1 or 2" ) )
          )
      ),
      validateField( "option", option ),
      validateField( "value3", value3 ),
      validateField( "nestedSeq", nestedSeq )
    ).mapN( TestOuter )
  }

  "Validator" should "return Valid for correct values" in {
    val inner = TestInner( 1234.0 )
    val outer = TestOuter( 3, "abc", inner, Seq( 1, 2, 1, 1 ), Some( "xyz" ), "def", Seq( inner, inner ) )
    validate( outer ) shouldBe Valid( outer )
  }

  "Validator" should "return Invalid for incorrect values" in {
    val inner = TestInner( 123.0 )
    val outer = TestOuter( 123, "    ", inner, Seq( 4, 7 ), Some( "N/A" ), "null", Seq( inner ) )
    validate( outer ) shouldBe Invalid(
      NonEmptyList.of(
        "value1: must be less than 5",
        "value2: must not be empty or blank",
        "inner.value: must be greater than or equal to 1000",
        "seq: size must be greater than 3",
        "seq[0]: must be 1 or 2",
        "seq[1]: must be 1 or 2",
        "option: must not be disallowed string",
        "value3: must not be disallowed string",
        "nestedSeq[0].value: must be greater than or equal to 1000"
      ) )
  }

  "Validator.apply" should "create a validator that applies value to test and if false returns failure string" in {
    val v = Validator[String]( _.isBlank == false, "must not be blank" )
    v( "" ) shouldBe "must not be blank".invalidNel
  }

  "Validator.and" should "combine any errors from two Validators" in {
    val v1 = Validator[String]( _.contains( "a" ), "must contain 'a'" )
    val v2 = Validator[String]( _.contains( "b" ), "must contain 'b'" )
    val v3 = Validator.and( v1, v2 )

    v3( "ab" ).isValid shouldBe true
    v3( "b" ) shouldBe NonEmptyList.one( "must contain 'a'" ).invalid
    v3( "a" ) shouldBe NonEmptyList.one( "must contain 'b'" ).invalid
    v3( "" ) shouldBe NonEmptyList.of( "must contain 'a'", "must contain 'b'" ).invalid

    val v4 = v1.and( v2 )
    v4( "" ) shouldBe NonEmptyList.of( "must contain 'a'", "must contain 'b'" ).invalid
  }

  "Validator.errorIsScoped" should "return TRUE if error message is already scoped" in {
    Validator.errorIsScoped( "must not be blank" ) shouldBe false
    Validator.errorIsScoped( "value1: must not be blank" ) shouldBe true
    Validator.errorIsScoped( "value1[0].value2: must not be blank" ) shouldBe true
    Validator.errorIsScoped( "value1[0].value2 must not be blank" ) shouldBe true
  }

  "Validator.prependScopeToError" should "prepend scope string to error message " in {
    Validator.prependScopeToError( "scope", "message" ) shouldBe "scope: message"
  }

  it should "prepend scope string to error message with scope" in {
    Validator.prependScopeToError( "scope2", "scope: message" ) shouldBe "scope2.scope: message"
  }

  it should "prepend scope string to error message with index scope" in {
    Validator.prependScopeToError( "scope2", "[0]: message" ) shouldBe "scope2[0]: message"
  }

  "Validator[String]" should "return Invalid for invalid strings" in {
    stringValidator( "" ) shouldBe "must not be empty or blank".invalidNel
    stringValidator( "  " ) shouldBe "must not be empty or blank".invalidNel
    stringValidator( "n/a" ) shouldBe "must not be disallowed string".invalidNel
    stringValidator( "N/A" ) shouldBe "must not be disallowed string".invalidNel
    stringValidator( "null" ) shouldBe "must not be disallowed string".invalidNel
  }

  "Validator[Option[A]]" should "forward to Validator[A] for Some and return Valid for None" in {
    optionValidator[String]( stringValidator )( None ) shouldBe None.valid
    optionValidator[String]( stringValidator )( Some( "" ) ) shouldBe "must not be empty or blank".invalidNel
    Validator.option[String].apply( None ) shouldBe None.valid
    Validator.option[String].apply( Some( "" ) ) shouldBe "must not be empty or blank".invalidNel
  }

  "Validator[Seq[A]]" should "forward to Validator[A] and prepend scope to error messages" in {
    seqValidator[String]( stringValidator )( Seq.empty ) shouldBe Seq.empty.valid
    seqValidator[String]( stringValidator )( Seq( "abc123", "", "null" ) ) shouldBe
      NonEmptyList.of( "[1]: must not be empty or blank", "[2]: must not be disallowed string" ).invalid
    Validator.seq[String].apply( Seq.empty ) shouldBe Seq.empty.valid
    Validator.seq[String].apply( Seq( "abc123", "", "null" ) ) shouldBe
      NonEmptyList.of( "[1]: must not be empty or blank", "[2]: must not be disallowed string" ).invalid
  }

  "validateField" should "forward to Validator[A] and prepend field name scope to error messages" in {
    validateField( "test", "" )( stringValidator ) shouldBe "test: must not be empty or blank".invalidNel
  }

}
