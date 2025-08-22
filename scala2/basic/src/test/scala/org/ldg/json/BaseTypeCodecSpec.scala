package org.ldg.json

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BaseTypeCodecSpec extends AnyWordSpec with Matchers {

  sealed trait Animal
  case class Dog( name: String, breed: String, `type`: String ) extends Animal
  case class Cat( name: String, lives: Int ) extends Animal
  case class Bird( name: String, canFly: Boolean ) extends Animal

  implicit val dogCodec: Codec[Dog] = io.circe.generic.semiauto.deriveCodec
  implicit val catCodec: Codec[Cat] = io.circe.generic.semiauto.deriveCodec
  implicit val birdCodec: Codec[Bird] = io.circe.generic.semiauto.deriveCodec

  val encodeAnimal: Animal => ( String, Json ) = {
    case d: Dog  => ( "dog", d.asJson )
    case c: Cat  => ( "cat", c.asJson )
    case b: Bird => ( "bird", b.asJson )
  }

  val decodeAnimal: ( String, Json ) => Decoder.Result[Animal] = {
    case ( "dog", json )  => json.as[Dog]
    case ( "cat", json )  => json.as[Cat]
    case ( "bird", json ) => json.as[Bird]
    case ( unknown, _ )   => Left( io.circe.DecodingFailure( s"Unknown animal type: $unknown", Nil ) )
  }

  implicit val animalCodec: Codec[Animal] = new BaseTypeCodec[Animal, String]( encodeAnimal, decodeAnimal )

  def parseJson( json: String ): Json =
    parse( json ).fold( throw _, identity )

  "BaseTypeCodec" should {

    "encode animal with default type field" in {
      val dog = Dog( "Buddy", "Golden Retriever", "good boy" )
      val json = animalCodec( dog )

      json shouldBe parseJson( """
        |{
        | "name": "Buddy",
        | "breed": "Golden Retriever",
        | "type": "dog"
        |}""".stripMargin )
    }

    "encode animal with custom type field" in {
      val customCodec = new BaseTypeCodec[Animal, String]( encodeAnimal, decodeAnimal, "animalType" )
      val cat = Cat( "Whiskers", 9 )
      val json = customCodec( cat )

      json shouldBe parseJson( """
        |{
        | "name": "Whiskers",
        | "lives": 9,
        | "animalType": "cat"
        |}""".stripMargin )
    }

    "decode animal from valid JSON" in {
      val json = parseJson( """
        |{
        | "name": "Tweety",
        | "canFly": true,
        | "type": "bird"
        |}""".stripMargin )

      val result = json.as[Animal]
      result shouldBe Right( Bird( "Tweety", true ) )
    }

    "overwrite existing type field in subtype JSON" in {
      val dog = Dog( "Rex", "Labrador", "good boy" )
      val encoded = animalCodec( dog )

      encoded.hcursor.get[String]( "type" ) shouldBe Right( "dog" )
    }

    "fail encoding when subtype JSON is not an object" in {
      val invalidEncodeFunction: Animal => ( String, Json ) = _ => ( "dog", Json.fromString( "not an object" ) )
      val invalidCodec = new BaseTypeCodec[Animal, String]( invalidEncodeFunction, decodeAnimal )

      val dog = Dog( "Buddy", "Golden Retriever", "good boy" )
      an[IllegalArgumentException] should be thrownBy invalidCodec( dog )
    }

    "fail decoding when type field is missing" in {
      val json = parseJson( """
        |{
        | "name": "Buddy",
        | "breed": "Golden Retriever"
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when type field has wrong type" in {
      val json = parseJson( """
        |{
        | "name": "Buddy",
        | "breed": "Golden Retriever",
        | "type": 123
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when subtype key is unknown" in {
      val json = parseJson( """
        |{
        | "name": "Nemo",
        | "type": "fish"
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when subtype JSON structure is invalid" in {
      val json = parseJson( """
        |{
        | "name": "Buddy",
        | "breed": 123,
        | "type": "dog"
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "round trip encode and decode successfully" in {
      val originalCat = Cat( "Mittens", 7 )
      val encoded = animalCodec( originalCat )
      val decoded = encoded.as[Animal]( animalCodec )

      decoded shouldBe Right( originalCat )
    }

  }

}
