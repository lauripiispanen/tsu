package tsu

package object argonaut {
  import _root_.argonaut.Json, Json._
  import _root_.argonaut.DecodeJson

  implicit class JsonValueArgonaut(v: JsonValue) {
    def toJson: Json = v match {
      case JsonObject(fields) => obj(fields.map(f => (f.key, f.value.toJson)): _*)
      case JsonArray(values) => array(values.map(v => v.toJson): _*)
      case JsonString(s) => jString(s)
      case JsonNumber(n) => jNumber(n.toDouble)
      case JsonBoolean(b) => jBool(b)
      case JsonNull => jNull
    }

    def to[A](implicit d: DecodeJson[A]): A = toJson.as[A](d).toOption.get
  }
}
