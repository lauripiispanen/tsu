import com.fasterxml.jackson.core.{JsonToken, JsonParser, JsonFactory}
import scala.io.Source

package object tsu {

  abstract class JsonValue

  case class JsonObject(fields: Stream[JsonField]) extends JsonValue

  case class JsonField(key: String, value: JsonValue)

  case class JsonArray(contents: Stream[JsonValue]) extends JsonValue

  case class JsonString(value: String) extends JsonValue

  case class JsonNumber(value: BigDecimal) extends JsonValue

  case class JsonBoolean(value: Boolean) extends JsonValue

  case object JsonNull extends JsonValue

  def parse(source: Source): JsonValue = {
    parse(new SourceReader(source))
  }

  def parse(is: java.io.Reader): JsonValue = {
    val parser: JsonParser = new JsonFactory().createParser(is)

    def parseJsonValue(token: JsonToken): JsonValue = {
      token match {
        case JsonToken.START_OBJECT => JsonObject(parseObject)
        case JsonToken.START_ARRAY => JsonArray(parseArray)
        case JsonToken.VALUE_STRING => JsonString(parser.getText)
        case JsonToken.VALUE_NUMBER_INT => JsonNumber(parser.getDecimalValue)
        case JsonToken.VALUE_NUMBER_FLOAT => JsonNumber(parser.getDecimalValue)
        case JsonToken.VALUE_TRUE => JsonBoolean(true)
        case JsonToken.VALUE_FALSE => JsonBoolean(false)
        case JsonToken.VALUE_NULL => JsonNull
        case t => throw new IllegalStateException(t.toString)
      }
    }
    
    def parseObject: Stream[JsonField] = {
      parser.nextToken match {
        case JsonToken.FIELD_NAME => {
          val key = parser.getText
          val jsonValue: JsonValue = parseJsonValue(parser.nextToken)
          JsonField(key, jsonValue) #:: { traverse(jsonValue); parseObject }
        }
        case JsonToken.END_OBJECT => Stream()
        case t => throw new IllegalStateException(t.toString)
      }
    }

    def parseArray: Stream[JsonValue] = {
      parser.nextToken match {
        case JsonToken.END_ARRAY => Stream()
        case t => {
          val jsonValue = parseJsonValue(t)
          jsonValue #:: { traverse(jsonValue); parseArray }
        }
      }
    }

    parseJsonValue(parser.nextToken)
  }

  private def traverse(v: JsonValue): Unit = v match {
    case JsonObject(fields) => fields.map(_.value).foreach(traverse)
    case JsonArray(values) => values.foreach(traverse)
    case _ => Unit
  }

  private class SourceReader(source: Source) extends java.io.Reader {
    def close = source.close

    def read(buf: Array[Char], offset: Int, len: Int): Int = {
      Stream.range(offset, offset + len)
        .map((idx: Int) => {
        if(source.hasNext)
          (idx, Some(source.next))
        else
          (idx, None)
      })
        .foldLeft(0)((len: Int, last: (Int, Option[Char])) => {
        last._2.map(buf.update(last._1, _)).map(_ => len + 1).getOrElse(len)
      })
    }
  }


  abstract class Query[A] extends Function[JsonValue, Stream[A]]{
    def apply(v: JsonValue): Stream[A]
  }

  implicit class QueryChain[A <: JsonValue](q: Query[A]) {
    def ->(q2: Query[A]): Query[A] = new Query[A] {
      def apply(v: JsonValue): Stream[A] = q(v).flatMap(q2)
    }
  }

  object Query {
    implicit class JsonValueQuery(v: JsonValue) {
      def query[A](q: Query[A]): Stream[A] = q(v)
    }

    class StructureMismatch extends Exception

    def constant[T](t: T): Query[T] = new Query[T] {
      def apply(v: JsonValue): Stream[T] = Stream(t)
    }

    def filter(f: JsonValue => Boolean): Query[JsonValue] = new Query[JsonValue] {
      def apply(v: JsonValue): Stream[JsonValue] = if(f(v)) Stream(v) else Stream()
    }

    def eq(i: BigDecimal) = filter(_.asInstanceOf[JsonNumber].value == i)
    def eq(s: String) = filter(_.asInstanceOf[JsonString].value == s)
    def eq(b: Boolean) = filter(_.asInstanceOf[JsonBoolean].value == b)
    def isNull = filter(_ match { case JsonNull => true; case _ => false })

    def field(key: String): Query[JsonValue] = new Query[JsonValue] {
      def apply(v: JsonValue): Stream[JsonValue] = v match {
        case JsonObject(fields) => fields.filter(_.key == key).take(1).map(_.value)
        case _ => throw new StructureMismatch
      }
    }

    def where[A](q: Query[A]): Query[JsonValue] = new Query[JsonValue] {
      def apply(v: JsonValue): Stream[JsonValue] = q(v).take(1).map(_ => v)
    }

    def select[A](q: Query[A]) : Query[A] = new Query[A] {
      def apply(v: JsonValue): Stream[A] = v match {
        case JsonArray(values) => values.flatMap(q)
        case _ => throw new StructureMismatch
      }
    }
  }


}
