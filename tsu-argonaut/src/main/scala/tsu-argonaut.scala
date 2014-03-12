package tsu

package object argonaut {
  import _root_.argonaut._
  import Json._

  implicit class TsuToArgonaut(v: tsu.JsonValue) {
    private lazy val ast: Json = v match {
      case tsu.JsonObject(members) => obj(members.map(m => (m.key, m.value.toArgonautAst)): _*)
      case tsu.JsonArray(elements) => array(elements.map(_.toArgonautAst): _*)
      case tsu.JsonString(s)       => jString(s)
      case tsu.JsonNumber(n)       => jNumber(n.toDouble)
      case tsu.JsonBoolean(b)      => jBool(b)
      case tsu.JsonNull            => jNull
    }

    def toArgonautAst: Json = ast
  }

  implicit def tsuToArgonautAst(v: tsu.JsonValue) = v.toArgonautAst

  implicit class ArgonautToTsu(v: Json) {
    private lazy val ast: tsu.JsonValue = v.fold[tsu.JsonValue](
                  tsu.JsonNull,
      b        => tsu.JsonBoolean(b),
      n        => tsu.JsonNumber(BigDecimal(n)),
      s        => tsu.JsonString(s),
      elements => tsu.JsonArray(elements.toList.map(_.toTsuAat)),
      members  => tsu.JsonObject(members.toList.map(m => Member(m._1, m._2.toTsuAat)))
    )

    def toTsuAat: tsu.JsonValue = ast
  }

  implicit def argounautToTsuAst(v: Json) = v.toTsuAat
}
