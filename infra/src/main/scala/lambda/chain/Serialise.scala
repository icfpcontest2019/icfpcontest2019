package lambda.chain

import lambda.contest.Booster
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat}

// We use this to serialise/deserialise the names of boosters
// See https://github.com/spray/spray-json/issues/200
class EnumJsonConverter[T <: scala.Enumeration](enu: T) extends RootJsonFormat[T#Value] {
  override def write(obj: T#Value): JsValue = JsString(obj.toString)

  override def read(json: JsValue): T#Value = {
    json match {
      case JsString(txt) => enu.withName(txt)
      case somethingElse => throw DeserializationException(s"Expected a value from enum $enu instead of $somethingElse")
    }
  }
}

object LambdaJsonProtocol extends DefaultJsonProtocol {
  implicit val boosterFormat = new EnumJsonConverter(Booster)
  implicit val transactionFormat = jsonFormat6(Transaction)
}

