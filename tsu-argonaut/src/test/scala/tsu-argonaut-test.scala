package tsu.argonaut

import org.scalatest._
import tsu._

class ArgonautTest extends FlatSpec with Matchers {
  import _root_.argonaut.Argonaut._
  "Conversion" should "produce correct result" in {
    assert(JsonString("string").as[String].toOption == Some("string"))
    assert(JsonNumber(1).as[Int].toOption == Some(1))
    assert(JsonNumber(BigDecimal("1.243")).as[Double].toOption == Some(1.243))
    assert(JsonBoolean(true).as[Boolean].toOption == Some(true))
  }
}
