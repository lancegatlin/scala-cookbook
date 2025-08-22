package org.ldg.json

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}

/**
  * A Codec for a base type (e.g. sealed trait) which has a number of subtypes (e.g. case classes) and which emits the
  * following JSON structure:
  * {{{
  *   {
  *     "<subtype key field>": "<subtype key JSON>",
  *     "<subtype field1>": <value1>,
  *     "<subtype field2>": <value2>,
  *     ...
  *   }
  * }}}
  * Note1: the subtype JSON must be a JSON object with fields, not just any valid JSON object
  * Note2: if subtype JsonObject contains a field with the same name as the subtype key field it will be overwritten
  * Note3: BaseTypeCodec differs from circe generic-extras Configuration.default.withDiscriminator since it allows a
  * custom type for the subtype key, not just String set to the subtype class name (see
  * https://circe.github.io/circe/codecs/adt.html)
  *
  * @param encodeSubtype a function that takes an instance of the base type (i.e. usually a subtype) and returns a
  *                      tuple of the subtype key and the JSON object representation of the subtype.
  * @param decodeSubtype a function that takes a subtype key and a JSON representation of the subtype and returns the
  *                      result of decoding the subtype from the JSON
  * @param subtypeKeyField the name of the field in the JSON which contains the subtype key
  * @param subtypeKeyDecoder a decoder for the subtype key
  * @param subtypeKeyEncoder an encoder for the subtype key
  * @tparam BaseType the base type of the ADT
  * @tparam SubtypeKey a type which represents the type of subtype which can encoded to JSON and later decoded from JSON
  *                    (e.g. String or an enum or Class, etc.)
  */
class BaseTypeCodec[BaseType, SubtypeKey](
    // note: due to quirk with Circe design we can't use JsonObject here, must use Json and then convert to JsonObject
    // inside the method
    encodeSubtype: BaseType => ( SubtypeKey, Json ),
    decodeSubtype: ( SubtypeKey, Json ) => Decoder.Result[BaseType],
    subtypeKeyField: String = BaseTypeCodec.defaultSubtypeKeyField
)( implicit subtypeKeyDecoder: Decoder[SubtypeKey], subtypeKeyEncoder: Encoder[SubtypeKey] )
    extends Codec[BaseType] {

  def apply( a: BaseType ): Json = {
    val ( subtypeKey, subtypeJson ) = encodeSubtype( a )
    val jsonObject = subtypeJson
      .asObject.getOrElse(
        throw new IllegalArgumentException( "Subtype JSON must be JsonObject" )
      )
    // note: append subtypeKey field to the existing fields in the JsonObject to overwrite any existing field with the same name
    val subtypeFields = jsonObject.toList ::: List( subtypeKeyField -> subtypeKey.asJson )
    Json.obj( subtypeFields: _* )
  }

  def apply( c: HCursor ): Decoder.Result[BaseType] =
    for {
      subtypeKey <- c.get[SubtypeKey]( subtypeKeyField )
      result <- decodeSubtype( subtypeKey, c.value )
    } yield result
}

object BaseTypeCodec {
  val defaultSubtypeKeyField = "type"
}
