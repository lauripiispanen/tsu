package tsu

package object argonaut {
  import _root_.argonaut._
  import Json._

  implicit class JsonValueArgonaut(v: tsu.JsonValue) {
    private def toJson: Json = v match {
      case tsu.JsonObject(fields) => obj(fields.map(f => (f.key, new JsonValueArgonaut(f.value).toJson)): _*)
      case tsu.JsonArray(values) => array(values.map(v => new JsonValueArgonaut(v).toJson): _*)
      case tsu.JsonString(s) => jString(s)
      case tsu.JsonNumber(n) => jNumber(n.toDouble)
      case tsu.JsonBoolean(b) => jBool(b)
      case tsu.JsonNull => jNull
    }

    def as[A: DecodeJson]: Option[A] = toJson.as[A].toOption
  }
}
