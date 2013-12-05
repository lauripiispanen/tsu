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
    def |>[C](q2: Query[B, C]) = new Query[A, C] {
      def apply(v: A): C = q2(Query.this(v))
    }
  }

  object Query {
    implicit class QueryableJsonValue(v: JsonValue) {
      def query[A](q: JsonValue => A): A = q(v)
      def \==(i: BigDecimal) = v.asInstanceOf[JsonNumber].value == i
      def \==(i: String) = v.asInstanceOf[JsonString].value == i
      def \==(i: Boolean) = v.asInstanceOf[JsonBoolean].value == i
      def isNull = v match { case JsonNull => true; case _ => false}
      def \(key: String) = v.asInstanceOf[JsonObject].fields.filter(_.key == key).head.value
      def map(f: JsonValue => JsonValue) = Query.map(f)(v)
      def filter(f: JsonValue => Boolean) = Query.filter(f)(v)
      def contents = Query.contents(v)
      def flatten = Query.flatten(v)
    }

    def map(f: JsonValue => JsonValue) = new Query[JsonValue, JsonArray] {
      def apply(v: JsonValue): JsonArray = JsonArray(v.asInstanceOf[JsonArray].contents.map(f))
    }

    def filter(f: JsonValue => Boolean) = new Query[JsonValue, JsonArray] {
      def apply(v: JsonValue): JsonArray = JsonArray(v.asInstanceOf[JsonArray].contents.filter(f))
    }

    def contents = new Query[JsonValue, Stream[JsonValue]] {
      def apply(v: JsonValue): Stream[JsonValue] = v.asInstanceOf[JsonArray].contents
    }

    def flatten = new Query[JsonValue, JsonArray] {
      def apply(v: JsonValue): JsonArray = JsonArray(v.asInstanceOf[JsonArray].contents.flatMap(_.asInstanceOf[JsonArray].contents))
    }

    def constant[T](t: T) = new Query[JsonValue, T] {
      def apply(v: JsonValue): T = t
    }

    implicit def \(key: String) = new JsonValueQuery[JsonValue] {
      def apply(v: JsonValue): JsonValue = v \ key
    }

    abstract class JsonValueQuery[A] extends Query[A, JsonValue]{
      def \==(i: BigDecimal) = new Query[A, Boolean] {
        def apply(v: A) = JsonValueQuery.this(v) \== i
      }
      def \==(i: String) = new Query[A, Boolean] {
        def apply(v: A) = JsonValueQuery.this(v) \== i
      }
      def \==(i: Boolean) = new Query[A, Boolean] {
        def apply(v: A) = JsonValueQuery.this(v) \== i
      }
      def isNull = new Query[A, Boolean] {
        def apply(v: A) = JsonValueQuery.this(v) isNull
      }
    }

  }
}
