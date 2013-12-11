package tsu.parser

import org.scalatest.{FlatSpec, Matchers}
import java.io.InputStream
import scala.io.Source
import scala.util.parsing.input.{CharArrayReader, StreamReader}
import tsu._

class ParserTest extends FlatSpec with Matchers {
  "Parser" should "not consume more input than necessary for requested data" in {
    val inputStream: InputStream = getClass.getResourceAsStream("/people.json")
    val friend: JsonParser[JsonObject] =
      `object`(
        "id" -> number(2, 1, 1),
        "name" -> string(200)
      )
    val person: JsonParser[JsonObject] =
      `object`(
        "id" -> number(2, 1, 1),
        "name" -> string(200),
        "gender" -> string(200),
        "company" -> string(200),
        "friends" -> array(200, friend)
      )
    val people: JsonParser[JsonArray] = array(7, person)

    val result = people.allowWhitespace(200)(StreamReader(Source.fromInputStream(inputStream, "UTF-8").reader))

    result.flatMap(_.elements.take(6))
  }

  "Parser" should "process structural json correctly" in {
    val p = `object`(
      "a\"" -> number(2,1,2),
      "b\"" -> string(2),
      "c\"" -> `object`(
        "a" -> number(1,0,0),
        "c" -> number(1,0,0),
        "b" -> string(1)
      ),
      "d" -> ( boolean | `null` )
    )
    val raw1 = p.allowWhitespace(3)(new CharArrayReader( """ { "a\"" : -12.2E22, "c\"" : { "b" : "0", "a" : 0, "c" : 0 }, "b\"" : "a\"", "d": null } """.trim.toCharArray))
    raw1.flatMap(_.members).foreach(println)

    val a2 = array(4, string(2))

    val raw3 = a2.allowWhitespace(3)(new CharArrayReader( """ [ "a\"" , "a\"", "b\"", "a\"" ] """.trim.toCharArray))

    for {
      arr <- raw3
      element <- arr.elements
    } {
      println(element)
    }
  }
}