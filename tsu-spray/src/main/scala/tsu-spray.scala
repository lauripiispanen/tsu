package tsu

import scala.util.Try

package object spray {
  import _root_.spray.json._

  implicit class TsuToSprayJson(v: tsu.JsonValue) {
    private lazy val ast: JsValue =  v match {
      case tsu.JsonObject(members) => JsObject(members.map(m => (m.key, m.value.toSprayJsonAst)): _*)
      case tsu.JsonArray(elements) => JsArray(elements.map(_.toSprayJsonAst): _*)
      case tsu.JsonString(s)       => JsString(s)
      case tsu.JsonNumber(n)       => JsNumber(n)
      case tsu.JsonBoolean(b)      => JsBoolean(b)
      case tsu.JsonNull            => JsNull
    }

    def toSprayJsonAst: JsValue = ast

    def tryConvertTo[A: JsonReader]: Try[A] = Try(ast.convertTo[A])
  }

  implicit def tsuToSprayJsonAst(v: tsu.JsonValue): JsValue = v.toSprayJsonAst

  implicit class SprayJsonToTsu(v: JsValue) {
    private lazy val ast: tsu.JsonValue = v match {
      case JsObject(members) => tsu.JsonObject(members.toSeq.map(m => tsu.Member(m._1, m._2.toTsuAst)))
      case JsArray(elements) => tsu.JsonArray(elements.toSeq.map(_.toTsuAst))
      case JsString(s)       => tsu.JsonString(s)
      case JsNumber(n)       => tsu.JsonNumber(n)
      case JsBoolean(b)      => tsu.JsonBoolean(b)
      case JsNull            => tsu.JsonNull
    }

    def toTsuAst: tsu.JsonValue = ast
  }

  implicit def sprayToTsuAst(v: JsValue): tsu.JsonValue = v.toTsuAst
}
