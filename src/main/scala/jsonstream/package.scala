import com.fasterxml.jackson.core.{JsonToken, JsonParser, JsonFactory}
import java.io.Reader

package object jsonstream {

  abstract class JsonValue

  case class JsonObject(fields: Stream[JsonField]) extends JsonValue

  case class JsonField(key: String, value: JsonValue)

  case class JsonArray(contents: Stream[JsonValue]) extends JsonValue

  case class JsonString(value: String) extends JsonValue

  case class JsonNumber(value: BigDecimal) extends JsonValue

  case class JsonBoolean(value: Boolean) extends JsonValue

  case object JsonNull extends JsonValue

  def parse(is: Reader) = {
    val parser: JsonParser = new JsonFactory().createParser(is)

    def parseJsonValue(token: JsonToken): JsonValue = {
      token match {
        case JsonToken.START_OBJECT => JsonObject(parseObjectFields)
        case JsonToken.START_ARRAY => JsonArray(parseArrayValues)
        case JsonToken.VALUE_STRING => JsonString(parser.getText)
        case JsonToken.VALUE_NUMBER_INT => JsonNumber(parser.getDecimalValue)
        case JsonToken.VALUE_NUMBER_FLOAT => JsonNumber(parser.getDecimalValue)
        case JsonToken.VALUE_TRUE => JsonBoolean(true)
        case JsonToken.VALUE_FALSE => JsonBoolean(false)
        case JsonToken.VALUE_NULL => JsonNull
        case t => throw new IllegalStateException(t.toString)
      }
    }
    
    def parseObjectFields: Stream[JsonField] = {
      parser.nextToken match {
        case JsonToken.FIELD_NAME => {
          val key = parser.getText
          val jsonValue: JsonValue = parseJsonValue(parser.nextToken)
          JsonField(key, jsonValue) #:: { traverse(jsonValue); parseObjectFields }
        }
        case JsonToken.END_OBJECT => Stream()
        case t => throw new IllegalStateException(t.toString)
      }
    }

    def parseArrayValues: Stream[JsonValue] = {
      parser.nextToken match {
        case JsonToken.END_ARRAY => Stream()
        case t => {
          val jsonValue = parseJsonValue(t)
          jsonValue #:: { traverse(jsonValue); parseArrayValues }
        }
      }
    }

    parseJsonValue(parser.nextToken)
  }

  private def traverse(v: JsonValue): Unit = v match {
    case JsonObject(fields) => fields.foreach(f => traverse(f.value))
    case JsonArray(values) => values.foreach(traverse)
    case _ => Unit
  }
}
