package sfpbook.ch9

import scala.util.matching.Regex

case class SimpleParser[+A](run: Location => SimpleParser.Result[A])

object SimpleParser {
  sealed trait Result[+A] {
    def map[B >: A](f: A => B): Result[B]
    def mapError(f: ParseError => ParseError): Result[A]
    def uncommit: Result[A]
 }
  case class ParseSuccess[+A](get: A, charsConsumed: Int) extends Result[A] {
    override def map[B >: A](f: A => B): Result[B] = ParseSuccess(f(get), charsConsumed)
    override def mapError(f: ParseError => ParseError): Result[A] = this
    override def uncommit: Result[A] = this
  }
  case class ParseError(stack: List[(String, Location)], isCommited: Boolean) extends Result[Nothing] {
    override def map[B >: Nothing](f: Nothing => B): Result[B] = this
    override def mapError(f: ParseError => ParseError): Result[Nothing] = f(this)
    override def uncommit: Result[Nothing] = ParseError(stack, false)

    def last: (String, Location) = stack.head
    def lastLocation: Location = last._2
    def push(msg: String, l: Location = lastLocation): ParseError = ParseError((msg, l) :: stack, isCommited)

  }
  object ParseError {
    def apply(msg: String, l: Location): ParseError = ParseError((msg, l) :: Nil, true)
    def apply(msg: String, input: String, offset: Int): ParseError = ParseError(msg, Location(input, offset))
  }
}

object SimpleParsers extends Parsers[SimpleParser.ParseError, SimpleParser] {
  import SimpleParser._

  private implicit def sp[A](f: Location => Result[A]): SimpleParser[A] = SimpleParser(f)

  override def run[A](pa: SimpleParser[A])(input: String): Either[ParseError, A] = pa.run(Location(input, 0)) match {
    case ParseSuccess(a, _) => Right(a)
    case e: ParseError => Left(e)
  }

  override val noop = sp { l => ParseSuccess(l.after, l.after.length) }

  override def delay[A](pa: => SimpleParser[A]) = sp { pa.run }
  override def attempt[A](pa: SimpleParser[A]) = sp { l => pa.run(l).uncommit }

  override def alwaysFail[A]= sp { ParseError("alwaysFail", _) }
  override def succeed[A](a: => A) = sp { _ => ParseSuccess(a, 0) }

  override implicit def string(s: String) = sp { l =>
    def diff: Int = s zip l.after indexWhere { case (x, y) => x != y }
    if (l.after.startsWith(s)) ParseSuccess(s, s.length)
    else ParseError(s"string mismatch ($s)", l + diff)
  }

  override implicit def regex(r: Regex) = sp { l =>
    r.findPrefixMatchOf(l.after)
      .map { m => ParseSuccess(m.matched, m.end) }
      .getOrElse { ParseError(s"regex mismatch $r", l) }
  }

  override protected[this] def or[A](p1: SimpleParser[A], p2: => SimpleParser[A]) = sp { l =>
    p1.run(l) match {
      case ParseError(_, false) => p2.run(l)
      case r => r
    }
  }

  override protected[this] def flatMap[A, B](p: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B] = sp { l =>
    p.run(l) match {
      case ParseSuccess(a, n) => f(a).run(l + n) match {
        case ParseSuccess(b, m) => ParseSuccess(b, n + m)
        case ParseError(s, isCommited) => ParseError(s, isCommited || n > 0)
      }
      case e: ParseError => e
    }
  }

  override protected[this] def slice[A](pa: SimpleParser[A]): SimpleParser[String] = sp { l =>
    pa.run(l) match {
      case ParseSuccess(_, n) => ParseSuccess(l.after.substring(0, n), n)
      case e: ParseError => e
    }
  }

  override def label[A](msg: String)(p: SimpleParser[A]): SimpleParser[A] = sp { l =>
    p.run(l).mapError {
      case ParseError((_, loc) :: t, isCommited) => ParseError((msg, loc) :: t, isCommited)
      case ParseError(Nil, _) => sys.error("Unexpected. Should be a non empty-list")
    }
  }

  override def scope[A](msg: String)(p: SimpleParser[A]): SimpleParser[A] = sp { l =>
    p.run(l).mapError { _.push(msg) }
  }

  override def errorLocation(e: ParseError): Location = e.stack.head._2
  override def errorMessage(e: ParseError): String = e.stack.head._1
}

