import scala.annotation.tailrec
import scala.util.Try

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

    class ErrorInStreamException(msg: String) extends Exception(msg)

    def toSeq: scala.collection.immutable.Stream[A] = {
      import scala.collection.immutable.Stream.{Cons, Empty}
      this match {
        case a: Next[A] => new Cons(a.value, a.rest.toSeq)
        case Error(msg) => throw new ErrorInStreamException(msg)
        case End => Empty
      }
    }

    @tailrec
    final def foreach[B](f: A => B): Unit ={
      val result = this match {
        case Next(value, rest) => {
          f(value)
          (true, rest)
        }
        case Error(message) => throw new ErrorInStreamException(message)
        case s => (false, s)
      }
      if(result._1) result._2.foreach(f)
    }

    def take(n: Int): Stream[A] = handleTermination { a =>
      if(n <= 0)
        End
      else if (n == 1)
        Next(a.value, End)
      else
        Next(a.value, a.rest.take(n - 1))
    }

    @tailrec
    final def filter(f: A => Boolean): Stream[A] = {
      val result = this match {
        case n: Next[A] => if(f(n.value)) (true, Next(n.value, Stream.filter(n.rest, f))) else (false, n.rest)
        case s => (true, s)
      }
      if(result._1) result._2 else result._2.filter(f)
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
    implicit def seqToStream[A](as: Seq[A]): Stream[A] = {
      if(as.isEmpty) {
        End
      } else {
        Next(as.head, seqToStream(as.tail))
      }
    }
    private def filter[A](s: Stream[A], f: A => Boolean): Stream[A] = {
      s.filter(f)
    }
    def apply[A](a: => A): Stream[A] = Try{ Next(a, End) } recover ErrorWithThrowable() getOrElse Error("Unknown error")
  }

  object Next {
    def apply[A](value: => A, r: => Stream[A]): Stream[A] = Try { new Next(value, r) } recover ErrorWithThrowable() getOrElse Error("Unknown error")
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

  final class Next[+A] protected (val value: A, r: => Stream[A]) extends Stream[A] {
    lazy val rest: Stream[A] = Try{ r } recover ErrorWithThrowable() getOrElse Error("Unknown error")
  }

  class Error(val message: String) extends Stream[Nothing]

  object Error {
    def apply(message: String) = new Error(message)
    def unapply[A](as: Stream[A]): Option[String] = {
      if(as.isInstanceOf[Error]) {
        Some(as.asInstanceOf[Error].message)
      } else {
        None
      }
    }
  }

  class ErrorWithThrowable(val throwable: Throwable) extends Error(throwable.getMessage)

  object ErrorWithThrowable {
    def apply[A](t: Throwable) = new ErrorWithThrowable(t)
    def apply[A](): PartialFunction[Throwable, Stream[A]] = { case t: Throwable => ErrorWithThrowable(t) }
    def unapply[A](as: Stream[A]): Option[Throwable] = {
      if(as.isInstanceOf[ErrorWithThrowable]) {
        Some(as.asInstanceOf[ErrorWithThrowable].throwable)
      } else {
        None
      }
    }
  }

  case object End extends Stream[Nothing]
}