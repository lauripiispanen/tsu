package tsu

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

    def idEq(i: BigDecimal) = field("id") -> Query.eq(i)

    val inputStream: InputStream = this.getClass.getResourceAsStream("/test_first_two_objects.json")
    val objects: Stream[JsonObject] =
      parse(Source.fromInputStream(inputStream, "UTF-8"))
      .query(select(where(idEq(0))))
      .map(_.asInstanceOf[JsonObject])

    assert(objects.head.fields.filter(_.key == "id").head.value.asInstanceOf[JsonNumber].value == 0)
  }
}
