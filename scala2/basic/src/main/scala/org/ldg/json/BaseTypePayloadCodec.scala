package org.ldg.json

import io.circe._
import io.circe.syntax.EncoderOps

/**
  * A Codec for a base type (e.g. sealed trait) which has a number of subtypes (e.g. case classes) and which emits the
  * following JSON structure:
  * {{{
  *   {
  *     "<subtype key field>": <subtype key JSON>,
  *     "<payload field>": <subtype JSON>
  *   }
  * }}}
  * Note1: the payload JSON may be any valid JSON object, not just a JSON object with fields
  * Note2: BaseTypePayloadCodec differs from circe generic-extras Configuration.default.withDiscriminator since it
  * allows a custom type for the subtype key, not just String set to the subtype class name (see
  * https://circe.github.io/circe/codecs/adt.html) and wraps the subtype JSON in a payload field allowing subtype JSON
  * to be any valid JSON object
  *
  * @param encodeSubtype a function that takes an instance of the base type (i.e. usually a subtype) and returns a
  *                      tuple of the subtype key and the JSON representation of the subtype
  * @param decodeSubtype a function that takes a subtype key and a JSON representation of the subtype and returns the
  *                      result of decoding the subtype from the JSON
  * @param subtypeKeyDecoder a decoder for the subtype key
  * @param subtypeKeyEncoder an encoder for the subtype key
  * @tparam BaseType the base type of the ADT
  * @tparam SubtypeKey a type which represents the type of subtype which can encoded to JSON and later decoded from JSON
  *                    (e.g. String or an enum or Class, etc.)
  */
class BaseTypePayloadCodec[BaseType, SubtypeKey](
    encodeSubtype: BaseType => ( SubtypeKey, Json ),
    decodeSubtype: ( SubtypeKey, Json ) => Decoder.Result[BaseType],
    subtypeField: String = BaseTypePayloadCodec.defaultSubtypeKeyField,
    payloadField: String = BaseTypePayloadCodec.defaultPayloadField
)( implicit subtypeKeyDecoder: Decoder[SubtypeKey], subtypeKeyEncoder: Encoder[SubtypeKey] )
    extends Codec[BaseType] {

  def apply( a: BaseType ): Json = {
    val ( subtypeKey, payloadJson ) = encodeSubtype( a )
    Json.obj( subtypeField -> subtypeKey.asJson, payloadField -> payloadJson )
  }

  def apply( c: HCursor ): Decoder.Result[BaseType] =
    for {
      subtypeKey <- c.get[SubtypeKey]( subtypeField )
      result <- c.get[Json]( payloadField ).flatMap { payloadJson =>
        decodeSubtype( subtypeKey, payloadJson )
      }
    } yield result
}

object BaseTypePayloadCodec {
  val defaultSubtypeKeyField = "type"
  val defaultPayloadField = "payload"
}
