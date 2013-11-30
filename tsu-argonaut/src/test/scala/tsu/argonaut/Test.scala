package tsu.argonaut

import org.scalatest._
import tsu._

class Test extends FlatSpec with Matchers {
  "Conversion" should "produce correct result" in {
    import _root_.argonaut.Argonaut._
    assert(JsonString("lol").to[String] == "lol")
  }
}
