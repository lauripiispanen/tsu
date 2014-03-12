package tsu.parser

import org.scalatest.{FlatSpec, Matchers}
import java.io.InputStream
import scala.io.Source
import scala.util.parsing.input._
import tsu._

class ParserTest extends FlatSpec with Matchers {

  val person = {
    val friend =
      `object`(
        "id" -> number(2, 1, 1),
        "name" -> string(200)
      )
    `object`(
      "id" -> number(2, 1, 1),
      "name" -> string(200),
      "gender" -> string(200),
      "company" -> string(200),
      "friends" -> array(200, friend)
    )
  }

  "Parser" should "not consume more input than necessary for requested data" in {
    val inputStream: InputStream = getClass.getResourceAsStream("/people_6_incomplete.json")

    val parser = array(6, person).allowWhitespace(200)

    parser(reader(inputStream))
      .flatMap(_.elements.take(6))
      .foreach(println)
  }

  it should "parse unicode escapes correctly" in {
    val ps = string("\uD834\uDD1E").allowWhitespace(0)
    ps(new CharArrayReader("\"\\uD834\\uDD1E\"".toCharArray)).foreach(println)
  }

  ignore should "be constant memory" in {
    val inputStream: InputStream = getClass.getResourceAsStream("/people_5.json")

    val parser  = array(5, person).allowWhitespace(200)

    val r = reader(inputStream)

    parser(new RepeatingReader(r, r))
      .flatMap(_.elements)
      .map(_.asInstanceOf[JsonObject])
      .flatMap(_.members)
      .filter(_.key == "id")
      .foreach(m => assert(m.value.asInstanceOf[JsonNumber].value < 5))
  }

  private def reader(inputStream: InputStream): StreamReader = {
    StreamReader(Source.fromInputStream(inputStream, "UTF-8").reader)
  }
}

class RepeatingReader(r1: Reader[Char], r2: Reader[Char]) extends Reader[Char] {
  override def first: Char = if(r2.atEnd) r1.first else r2.first

  override def atEnd: Boolean = false

  override def pos: Position = if(r2.atEnd) r1.pos else r2.pos

  override def rest: Reader[Char] = if(r2.rest.atEnd) new RepeatingReader(r1, r1) else new RepeatingReader(r1, r2.rest)
}