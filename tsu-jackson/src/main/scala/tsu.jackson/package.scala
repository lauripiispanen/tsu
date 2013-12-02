package tsu

import scala.io.Source
import com.fasterxml.jackson.core.{JsonToken, JsonFactory, JsonParser}

package object jackson {
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

}
