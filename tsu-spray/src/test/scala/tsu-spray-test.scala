package tsu.spray

import org.scalatest._
import tsu._

class SprayJsonTest extends FlatSpec with Matchers {
  import _root_.spray.json.DefaultJsonProtocol._
  "Conversion" should "produce correct result" in {
    assert(JsonString("string").tryConvertTo[String].toOption == Some("string"))
    assert(JsonNumber(1).tryConvertTo[Int].toOption == Some(1))
    assert(JsonNumber(BigDecimal("1.243")).tryConvertTo[BigDecimal].toOption == Some(BigDecimal("1.243")))
    assert(JsonBoolean(true).tryConvertTo[Boolean].toOption == Some(true))
  }
}
