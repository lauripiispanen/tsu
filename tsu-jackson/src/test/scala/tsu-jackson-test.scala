package tsu.jackson

import tsu._
import org.scalatest._
import java.io.InputStream
import scala.io.Source

class Test extends FlatSpec with Matchers {
  "Parser" should "not consume more input than necessary for requested data" in {
    val inputStream: InputStream = getClass.getResourceAsStream("/test_first_two_objects.json")
    val value = parse(Source.fromInputStream(inputStream, "UTF-8"))
    value match {
      case Next(JsonArray(fields), _) => {
        fields.take(2).foreach(println)
      }
      case _ => fail
    }
  }
}