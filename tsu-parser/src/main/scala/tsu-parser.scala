package tsu

import scala.util.parsing.input.Position

package object parser {
import scala.util.parsing.input.Reader

  abstract class JsonParser[+T] {
    def apply(allowedWhitespace: Int): Reader[Char] => Stream[T] = r => parse(new TsuParser(allowedWhitespace))(new Rd(r))

    private def parse(tsuParser: TsuParser): Reader[Either[TsuParser.NoSuccess, Char]] => Stream[T] = { in =>
      if(in.atEnd) {
        End
      } else {
        apply(tsuParser)(in) match {
          case s: TsuParser.Success[T] => Next(s.result, parse(tsuParser)(s.next))
          case n: TsuParser.NoSuccess => Error(n.toString)
        }
      }
    }

    private class Rd(r: Reader[Char]) extends Reader[Either[TsuParser.NoSuccess, Char]] {
      override def atEnd: Boolean = r.atEnd
      override def pos: Position = r.pos
      override def rest: Reader[Either[TsuParser.NoSuccess, Char]] = new Rd(r.rest)
      override def first: Either[TsuParser.NoSuccess, Char] = Right(r.first)
    }

    protected[parser] def apply(tsuParser: TsuParser): TsuParser.Parser[T]
  }

  object JsonParser {
    implicit class ParserConfiguration[+T <: JsonValue](p: JsonParser[T]) {
      def allowWhitespace(i: Int): Reader[Char] => Stream[T] = p(i)
    }
    implicit class JsonParserCombinations[T <: JsonValue](p1: JsonParser[T]) {
      def |[A <: JsonValue](p2: JsonParser[A]) = {
        new JsonParser[JsonValue] {
          def apply(tsuParser: TsuParser): TsuParser.Parser[JsonValue] = 
            p1(tsuParser) | p2.apply(tsuParser)
        }
      }
    }
  }

  implicit def stringAndValueParserTuple2FieldParser[T <: JsonValue](p: (JsonParser[JsonString], JsonParser[T])): JsonParser[Member] = new JsonParser[Member] {
    def apply(tsuParser: TsuParser): TsuParser.Parser[Member] = tsuParser.pair(p._1(tsuParser), p._2(tsuParser))
  }

  implicit def stringParserTuple2FieldParser(p: (String, JsonParser[JsonValue])): JsonParser[Member] =
    stringAndValueParserTuple2FieldParser((string(p._1), p._2))

  implicit def stringStringTuple2FieldParser(t: (String, String)): JsonParser[Member] =
    stringParserTuple2FieldParser((t._1, string(t._2)))

  private def wrap[T <: JsonValue](f: TsuParser => TsuParser.Parser[T]): JsonParser[T] = new JsonParser[T] {
    def apply(tsuParser: TsuParser): TsuParser.Parser[T] = f(tsuParser)
  }

  def `null`: JsonParser[JsonNull.type] = wrap(_.`null`)
  def boolean: JsonParser[JsonBoolean] = wrap(_.boolean)
  def boolean(b: Boolean): JsonParser[JsonBoolean] = wrap(_.boolean(b))
  def number(i: Int, f: Int, e: Int): JsonParser[JsonNumber] = wrap(_.number(i, f, e))
  def string(maxLen: Int): JsonParser[JsonString] = wrap(_.string(maxLen))
  def string(s: String): JsonParser[JsonString] = wrap(_.string(s))
  def array(maxLen: Int = 0, p: JsonParser[JsonValue]) = wrap(tsuParser => tsuParser.array(maxLen, p(tsuParser)))
  def `object`(ps: JsonParser[Member]*) = wrap(tsuParser => tsuParser.`object`(ps.map(_(tsuParser)):_*))


  import scala.util.parsing.combinator.{ImplicitConversions, Parsers}

  protected object TsuParser extends Parsers with ImplicitConversions {
    import scala.collection.immutable.Stream.range

    type Elem = Either[NoSuccess, Char]

    private val hexDigits = "0123456789abcdefABCDEF".toSet
    private val controlCharacters = "\"\\".toSet ++ range(0, 31).map(_.toChar)

    implicit def accept(e: Char): Parser[Elem] = accept(Right(e))

    override def Parser[T](f: Input => ParseResult[T]): Parser[T]
      = new Parser[T]{ def apply(in: Input) = if(in.first.isRight) f(in) else in.first.left.get }

    def char = {
      def allowedCharacter = elem("", _.right.toOption.map(c => !controlCharacters.contains(c)).getOrElse(false))

      def hexDigit: Parser[Elem] = elem("hex digit", _.right.toOption.map(hexDigits.contains).getOrElse(false))

      def codePoint: Parser[Elem] = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
        case Right(a) ~ Right(b) ~ Right(c) ~ Right(d) =>
          Right(Integer.parseInt(List(a, b, c, d) mkString "", 16).toChar)
      }

      def characterEscape: Parser[Elem] =
        '\\' ~> '\"'                |
        '\\' ~> '\\'                |
        '\\' ~> '/'                 |
        '\\' ~> 'b' ^^^ Right('\b') |
        '\\' ~> 'f' ^^^ Right('\f') |
        '\\' ~> 'n' ^^^ Right('\n') |
        '\\' ~> 'r' ^^^ Right('\r') |
        '\\' ~> 't' ^^^ Right('\t') |
        '\\' ~> 'u' ~> codePoint

      allowedCharacter | characterEscape
    }

    private val whitespaceCharacters = "\t\n\r ".toSet

    def whitespace(n: Int): Parser[Unit] = {
      def whitespaceChar = {
        elem("whitespace char", _.right.toOption.map(whitespaceCharacters.contains).getOrElse(false))
      }

      if (n < 0)
        err("Maximum allowed whitespace exceeded")
      else
        whitespaceChar ~> whitespace(n - 1) | success(Unit)
    }

    def number(i: Int, f: Int, e: Int) = {
      def digit = elem("digit", _.right.toOption.map(_.isDigit).getOrElse(false))

      def digits(memo: String, i: Int): Parser[String] =
        if(i == 0)
          not(digit) ^^^ memo
        else
          digit >> (d => digits(memo + d.right.get, i - 1)) | success(memo)

      def intPart(i: Int): Parser[String] = {
        def zero: Parser[String] = '0' ^^^ "0"
        def nonzero = elem("nonzero digit", _.right.toOption.map(d => d.isDigit && d != '0').getOrElse(false))

        zero | (nonzero >> (d => digits(d.right.get.toString, i - 1)))
      }

      def fracPart(i: Int): Parser[String] =
        '.' ~> (digit >> (d => digits(d.right.get.toString, i - 1)))

      def expPart(i: Int): Parser[String] = {
        def exponent = elem("exponent character", _.right.toOption.map(d => d == 'e' || d == 'E').getOrElse(false))

        exponent ~ opt(sign) ~ (digit >> (d => digits(d.right.get.toString, i - 1))) ^^ { case e ~ (s: Option[Elem]) ~ ds =>
          'E' +: optString("", s.map(_.right.get.toString)) ++: ds
        }
      }

      def optString(pre: String, a: Option[String]): String = a match {
        case Some(x) => pre + x
        case None => ""
      }

      def sign = elem("sign character", _.right.toOption.map(d => d == '-' || d == '+').getOrElse(false))

      opt('-') ~ intPart(i) ~ opt(fracPart(f)) ~ opt(expPart(e)) ^^ { case (s: Option[Elem]) ~ i ~ (f: Option[String]) ~ (e: Option[String]) =>
        optString("", s.map(_.right.get.toString)) + i + optString(".", f) + optString("", e)
      }
    }
  }

  protected class TsuParser(allowedWhitespace: Int) {
    import TsuParser._

    def pair(key: Parser[JsonString], value: Parser[JsonValue]): Parser[Member] =
      key >> (k => ws ~> ':' ~> ws ~> commit(value) ^^ (v => Member(k.value, v)))

    def `null`: Parser[JsonNull.type] = acceptSeq("null".map(c => Right(c): Elem)) ^^^ JsonNull

    def boolean(b: Boolean): Parser[JsonBoolean] =
      if(b)
        acceptSeq("true".map(c => Right(c): Elem)) ^^^ JsonBoolean(true)
      else
        acceptSeq("false".map(c => Right(c): Elem)) ^^^ JsonBoolean(false)

    def boolean: Parser[JsonBoolean] = boolean(true) | boolean(false)

    def number: (Int, Int, Int) => Parser[JsonNumber] = TsuParser.number(_, _, _) ^^ { s =>
      JsonNumber(BigDecimal(s))
    }

    def string(maxLen: Int): Parser[JsonString] = {
      def strMaxLenRecur(s: String, i: Int): TsuParser.Parser[String] =
        if (i <= 0)
          '\"' ^^^ s | failure("String length exceeded %d".format(maxLen))
        else
          char >> ( c => strMaxLenRecur(s + c.right.get , i - 1)) | '\"' ^^^ s

      '\"' ~> strMaxLenRecur("", maxLen) ^^ JsonString
    }

    def string(str: String): TsuParser.Parser[JsonString] = {
      def strRecur(s: String): Parser[String] =
        if (s.length == 0)
          '\"' ^^^ str | failure("String did not match %s".format(str))
        else
          char >> ( c => if (c.right.get == s.head) strRecur(s.tail) else failure("String did not match %s".format(str)))

      '\"' ~> strRecur(str) ^^ JsonString
    }

    private def emptyReader(ns: NoSuccess): Input = new Reader[Elem] {
      override def atEnd: Boolean = ns.next.atEnd

      override def pos: Position = ns.next.pos

      override def rest: Reader[TsuParser.Elem] = throw new IllegalStateException

      override def first: TsuParser.Elem = Left(ns)
    }

    def `object`(ps: Parser[Member]*): Parser[JsonObject] = {
      lazy val fail: Parser[Stream[Member]] = err("Unknown property")
      def stream(res: (Member, Parser[Stream[Member]])) = new Parser[Stream[Member]] {
        def apply(in: Input) = {
          val (field, members) = res
          lazy val rest = (ws ~> (',' ~> ws ~> members | '}' ^^^ End))(in)
          lazy val next = rest match { case s: Success[_] => s.next; case ns: NoSuccess => emptyReader(ns) }
          Success(Next(field, {
            rest match {
              case s: Success[Stream[Member]] => s.result
              case n: NoSuccess => tsu.Error(n.toString)
            }
          }), next)
        }
      }

      def members(ps: Parser[Member]*): Parser[Stream[Member]] =
        ps.map(p => p ^^ (v => (v, members(ps.filterNot(_ == p): _*))))
        .foldRight(fail)((parser, memo) => parser >> stream | memo )

      '{' ~> ws ~> (members(ps:_*) | '}' ^^^ End) ^^ JsonObject
    }

    def array(maxLen: Int = 0, value: Parser[JsonValue]): Parser[JsonArray] = {
      lazy val fail = failure("Array length exceeded %d".format(maxLen))
      def stream(i: Int)(v: JsonValue) = new Parser[Stream[JsonValue]] {
        def apply(in: Input) = {
          lazy val rest = (ws ~> (',' ~> ws ~> elements(i - 1) | ']' ^^^ End))(in)
          lazy val next = rest match { case s: Success[_] => s.next; case ns: NoSuccess => emptyReader(ns) }
          Success(Next(v, {
            rest match {
              case s: Success[Stream[JsonValue]] => s.result
              case n: NoSuccess => tsu.Error(n.toString)
            }
          }), next)
        }
      }

      def elements(i: Int): Parser[Stream[JsonValue]] =
        if(i <= 0)
          fail
        else
          value >> stream(i)

      '[' ~> ws ~> (elements(maxLen) | ']' ^^^ End) ^^ JsonArray
    }

    private def ws = whitespace(allowedWhitespace)

  }
}