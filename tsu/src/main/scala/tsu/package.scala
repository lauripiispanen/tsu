package object tsu {

  abstract class JsonValue

  case class JsonObject(fields: Stream[JsonField]) extends JsonValue

  case class JsonField(key: String, value: JsonValue)

  case class JsonArray(contents: Stream[JsonValue]) extends JsonValue

  case class JsonString(value: String) extends JsonValue

  case class JsonNumber(value: BigDecimal) extends JsonValue

  case class JsonBoolean(value: Boolean) extends JsonValue

  case object JsonNull extends JsonValue

  abstract class Query[-A, +B] extends Function[A, B]{
    def apply(v: A): B
  }

  implicit class QueryComposition[A, B](q: Query[A, B]) {
    def >[C](q2: Query[B, C]): Query[A, C] = new Query[A, C] {
      def apply(v: A): C = q2(q(v))
    }
  }

  object Query {
    implicit class JsonValueQuery(v: JsonValue) {
      def query[A](q: Query[JsonValue, A]): A = q(v)
    }

    class StructureMismatch extends Exception

    def map(f : JsonValue => JsonValue) = new Query[JsonArray, JsonArray] {
      def apply(v: JsonArray): JsonArray = JsonArray(v.contents.map(f))
    }

    def filter(f : JsonValue => Boolean) = new Query[JsonValue, JsonArray] {
      def apply(v: JsonValue): JsonArray = JsonArray(v.asInstanceOf[JsonArray].contents.filter(f))
    }

    def constant[T](t: T) = new Query[JsonValue, T] {
      def apply(v: JsonValue): T = t
    }

    def eq(i: BigDecimal) = new Query[JsonValue, Boolean] {
      def apply(v: JsonValue): Boolean = v.asInstanceOf[JsonNumber].value == i
    }

    def eq(i: String) = new Query[JsonValue, Boolean] {
      def apply(v: JsonValue): Boolean = v.asInstanceOf[JsonString].value == i
    }

    def eq(i: Boolean) = new Query[JsonValue, Boolean] {
      def apply(v: JsonValue): Boolean = v.asInstanceOf[JsonBoolean].value == i
    }

    def isNull = new Query[JsonValue, Boolean] {
      def apply(v: JsonValue): Boolean = v match { case JsonNull => true; case _ => false }
    }

    implicit def \(key: String) = new Query[JsonValue, JsonValue] {
      def apply(v: JsonValue): JsonValue = v.asInstanceOf[JsonObject].fields.filter(_.key == key).head.value
    }
    
  }


}
