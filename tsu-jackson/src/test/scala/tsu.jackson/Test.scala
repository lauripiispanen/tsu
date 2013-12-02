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
      case JsonArray(fields) => {
        fields.take(2).foreach(println)
      }
      case _ => fail
    }
  }

  "Query" should "return" in {
    import Query._

    val inputStream: InputStream = this.getClass.getResourceAsStream("/test_first_two_objects.json")
    val array: JsonArray =
      parse(Source.fromInputStream(inputStream, "UTF-8"))
      .query(filter(\("id") > Query.eq(0)) > map(\("id")))

    assert(array.contents.head.asInstanceOf[JsonNumber].value == 0)
  }
}
