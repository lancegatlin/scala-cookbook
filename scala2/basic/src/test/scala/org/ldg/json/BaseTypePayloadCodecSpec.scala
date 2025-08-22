package org.ldg.json

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BaseTypePayloadCodecSpec extends AnyWordSpec with Matchers {

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

  implicit val animalCodec: Codec[Animal] = new BaseTypePayloadCodec[Animal, String]( encodeAnimal, decodeAnimal )

  def parseJson( json: String ): Json =
    parse( json ).fold( throw _, identity )

  "BaseTypePayloadCodec" should {

    "encode animal with default type field" in {
      val dog = Dog( "Buddy", "Golden Retriever", "good boy" )
      val json = animalCodec( dog )

      json shouldBe parseJson( """
        |{
        | "type": "dog",
        | "payload": {
        |   "name": "Buddy",
        |   "breed": "Golden Retriever",
        |   "type": "good boy"
        | }
        |}""".stripMargin )
    }

    "encode animal with custom type and payload fields" in {
      val customCodec = new BaseTypePayloadCodec[Animal, String]( encodeAnimal, decodeAnimal, "animalType", "animalPayload" )
      val cat = Cat( "Whiskers", 9 )
      val json = customCodec( cat )

      json shouldBe parseJson( """
        |{
        | "animalType": "cat",
        | "animalPayload": {
        |  "name": "Whiskers",
        |  "lives": 9
        | }
        |}""".stripMargin )
    }

    "decode animal from valid JSON" in {
      val json = parseJson( """
        |{
        | "type": "bird",
        | "payload": {
        |   "name": "Tweety",
        |   "canFly": true
        | }
        |}""".stripMargin )

      val result = json.as[Animal]
      result shouldBe Right( Bird( "Tweety", true ) )
    }

    "fail decoding when type field is missing" in {
      val json = parseJson( """
        |{
        | "payload": {
        |   "name": "Buddy",
        |   "breed": "Golden Retriever",
        |   "type": "good boy"
        | }
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when type field has wrong type" in {
      val json = parseJson( """
        |{
        | "type": 123,
        | "payload": {
        |   "name": "Buddy",
        |   "breed": "Golden Retriever",
        |   "type": "good boy"
        | }
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when payload field is missing" in {
      val json = parseJson( """
        |{
        | "type": "dog"
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when subtype key is unknown" in {
      val json = parseJson( """
        |{
        | "type": "fish",
        | "payload": {
        |   "name": "Nemo"
        | }
        |}""".stripMargin )

      val result = json.as[Animal]( animalCodec )
      result.isLeft shouldBe true
    }

    "fail decoding when subtype JSON structure is invalid" in {
      val json = parseJson( """
        |{
        | "type": "dog",
        | "payload": {
        |   "name": "Buddy",
        |   "breed": 123
        | }
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
