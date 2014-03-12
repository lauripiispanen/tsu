package tsu.json4s

import org.scalatest._
import tsu._

class Json4sTest extends FlatSpec with Matchers {
  implicit val formats = org.json4s.DefaultFormats
  "Conversion" should "produce correct result" in {
    assert(JsonString("string").tryExtract[String].toOption == Some("string"))
    assert(JsonNumber(1).tryExtract[Int].toOption == Some(1))
    assert(JsonNumber(BigDecimal("1.243")).tryExtract[BigDecimal].toOption == Some(BigDecimal("1.243")))
    assert(JsonBoolean(true).tryExtract[Boolean].toOption == Some(true))
  }
}
