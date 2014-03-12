package tsu

package object json4s {
  import org.json4s._
  import scala.util.Try
  import scala.reflect.Manifest

  implicit class TsuToJson4s(v: tsu.JsonValue) {
    private lazy val ast: JValue = v match {
      case tsu.JsonObject(members) => JObject(members.map(m => (m.key, m.value.toJson4sAst)): _*)
      case tsu.JsonArray(elements) => JArray(elements.map(_.toJson4sAst).toList)
      case tsu.JsonString(s)       => JString(s)
      case tsu.JsonNumber(n)       => JDecimal(n)
      case tsu.JsonBoolean(b)      => JBool(b)
      case tsu.JsonNull            => JNull
    }

    def toJson4sAst: JValue = ast

    def tryExtract[A](implicit formats: Formats, mf: Manifest[A]): Try[A] = Try(Extraction.extract(toJson4sAst))
  }

  implicit def tsuToSprayJsonAst(v: tsu.JsonValue) = v.toJson4sAst

  implicit class Json4sToTsu(v: JValue) {
    private lazy val ast: tsu.JsonValue = v match {
      case JObject(members) => tsu.JsonObject(members.filterNot(JNothing ==).map(m => tsu.Member(m._1, m._2.toTsuAst)))
      case JArray(elements) => tsu.JsonArray(elements.filterNot(JNothing ==).map(_.toTsuAst))
      case JString(s)       => tsu.JsonString(s)
      case JDecimal(d)      => tsu.JsonNumber(d)
      case JInt(i)          => tsu.JsonNumber(BigDecimal(i))
      case JDouble(d)       => tsu.JsonNumber(BigDecimal(d))
      case JBool(b)         => tsu.JsonBoolean(b)
      case JNull            => tsu.JsonNull
      case JNothing         => throw new IllegalArgumentException("JNothing can not be translated to AST")
    }

    def toTsuAst: tsu.JsonValue = ast
  }

  implicit def jso4sToTsuAst(v: JValue) = v.toTsuAst
}
