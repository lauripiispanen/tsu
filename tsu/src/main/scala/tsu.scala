package object tsu {

  sealed abstract class JsonValue

  final case class Member(key: String, value: JsonValue)

  final case class JsonObject(members: Stream[Member]) extends JsonValue

  final case class JsonArray(elements: Stream[JsonValue]) extends JsonValue

  final case class JsonString(value: String) extends JsonValue

  final case class JsonNumber(value: BigDecimal) extends JsonValue

  final case class JsonBoolean(value: Boolean) extends JsonValue

  case object JsonNull extends JsonValue

  sealed abstract class Stream[+A] {
    def toSeq: scala.collection.immutable.Stream[A] = {
      import scala.collection.immutable.Stream.{Cons, Empty}
      this match {
        case a: Next[A] => new Cons(a.value, a.rest.toSeq)
        case End => Empty
      }
    }

    class ErrorInStreamException(msg: String) extends Exception(msg)

    def foreach[B](f: A => Unit): Unit = this match {
      case Next(v, rest) => {
        f(v)
        rest.foreach(f)
      }
      case e: Error => throw new ErrorInStreamException(e.message)
      case End => Unit
    }

    def take(n: Int): Stream[A] = handleTermination { a =>
      if(n <= 0)
        End
      else
        Next(a.value, if(n <= 1) End else a.rest.take(n - 1))
    }

    def filter(f: A => Boolean): Stream[A] = handleTermination { a =>
      if(f(a.value))
        Next(a.value, a.rest.filter(f))
      else
        a.rest.filter(f)
    }

    def map[B](f: A => B): Stream[B] = handleTermination { a =>
      Next(f(a.value), a.rest.map(f))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      def recur(b: B, bs: => Stream[B], as: => Stream[A]): Stream[B] =
        Next(b, {
          bs match {
            case n: Next[B] => recur(n.value, n.rest, as)
            case e: Error => e
            case End => as.flatMap(f)
          }
        })
      handleTermination { a =>
        f(a.value) handleTermination { b =>
          recur(b.value, b.rest, a.rest)
        }
      }
    }

    private def handleTermination[B](f: Next[A] => Stream[B]): Stream[B] = this match {
      case next: Next[A] => f(next)
      case e : Error => e
      case e @ End => e
    }
  }

  object Stream {
    implicit def streamToSeq[A](as: Stream[A]): Seq[A] = as.toSeq
  }

  object Next {
    def apply[A](value: A, r: => Stream[A]) = new Next(value, r)
    def unapply[A](as: Stream[A]): Option[(A, Stream[A])] = {
      if(as.isInstanceOf[Next[A]]) {
        val a = as.asInstanceOf[Next[A]]
        Some((a.value, a.rest))
      } else {
        None
      }
    }
    implicit def nextToValue[A](a: Next[A]): A = a.value
  }

  final class Next[+A](val value: A, r: => Stream[A]) extends Stream[A] {
    lazy val rest = r
  }

  final case class Error(message: String) extends Stream[Nothing]

  case object End extends Stream[Nothing]
}