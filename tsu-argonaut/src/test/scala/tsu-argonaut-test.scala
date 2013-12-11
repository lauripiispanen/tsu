package tsu.argonaut

import org.scalatest._
import tsu._

class Test extends FlatSpec with Matchers {
  "Conversion" should "produce correct result" in {
    assert(JsonString("string").as[String] == Some("string"))
    assert(JsonNumber(1).as[Int] == Some(1))
    assert(JsonNumber(BigDecimal("1.243")).as[Double] == Some(1.243))
    assert(JsonBoolean(true).as[Boolean] == Some(true))
  }
}
