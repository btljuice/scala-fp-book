package sfpbook.ch9

import scala.util.matching.Regex

case class SimpleParser[+A](run: String => SimpleParser.Result[A], attempt: Boolean = false)

object SimpleParser {
  sealed trait Result[+A] {
    def map[B >: A](f: A => B): Result[B]
    def mapError(f: ParseError => ParseError): Result[A]
 }
  case class ParseSuccess[+A](get: A, location: Location) extends Result[A] {
    override def map[B >: A](f: A => B): Result[B] = ParseSuccess(f(get), location)
    override def mapError(f: ParseError => ParseError): Result[A] = this
  }
  case class ParseError(stack: List[(String, Location)]) extends Result[Nothing] {
    override def map[B >: Nothing](f: Nothing => B): Result[B] = this
    override def mapError(f: ParseError => ParseError): Result[Nothing] = f(this)

    def last: (String, Location) = stack.head
    def lastLocation: Location = last._2
    def push(msg: String, l: Location = lastLocation): ParseError = ParseError((msg, l) :: stack)
  }
  object ParseSuccess {
    def apply[A](get: A, input: String, offset: Int = 0): ParseSuccess[A] = ParseSuccess(get, Location(input, offset))
  }
  object ParseError {
    def apply(msg: String, input: String, offset: Int = 0): ParseError = ParseError(msg, Location(input, offset))
    def apply(msg: String, l: Location): ParseError = ParseError((msg, l))
    def apply(entries: (String, Location)*): ParseError = ParseError(entries.toList)
  }
}

object SimpleParsers extends Parsers[SimpleParser.ParseError, SimpleParser] {
  import SimpleParser._

  private implicit def sp[A](f: String => Result[A]): SimpleParser[A] = SimpleParser(f)

  override def run[A](pa: SimpleParser[A])(input: String): Either[ParseError, A] = pa.run(input) match {
    case ParseSuccess(a, _) => Right(a)
    case e: ParseError => Left(e)
  }

  override val noop = sp { i => ParseSuccess(i, i, i.length) }

  override def delay[A](pa: => SimpleParser[A]) = sp { pa.run }
  override def attempt[A](pa: SimpleParser[A]) = SimpleParser(pa.run, attempt = true)

  override def alwaysFail[A]= sp { ParseError("alwaysFail", _) }
  override def succeed[A](a: => A) = sp { ParseSuccess(a, _) }

  override def string(s: String) = sp { i =>
    def diff: Int = s zip i indexWhere { case (x, y) => x != y }
    if (i.startsWith(s)) ParseSuccess(s, i, s.length)
    else ParseError(s"string mismatch ($s)", i, diff)
  }

  override implicit def regex(r: Regex) = sp { i =>
    r.findPrefixMatchOf(i)
      .map { m => ParseSuccess(m.matched, i, m.end) }
      .getOrElse { ParseError(s"regex mismatch $r", i) }
  }

  override protected[this] def or[A](p1: SimpleParser[A], p2: => SimpleParser[A]) = sp { i =>
    p1.run(i) match {
      case s: ParseSuccess[A] => s
      case _: ParseError if p1.attempt => p2.run(i)
      case e: ParseError => e
    }
  }

  override protected[this] def flatMap[A, B](p: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B] = sp { i =>
    p.run(i) match {
      case ParseSuccess(get, location) => f(get).run(location.after)
      case e: ParseError => e
    }
  }

  override protected[this] def slice[A](pa: SimpleParser[A]): SimpleParser[String] = sp { i =>
    pa.run(i) match {
      case ParseSuccess(_, location) => ParseSuccess(location.before, location)
      case e: ParseError => e
    }
  }

  override def label[A](msg: String)(p: SimpleParser[A]): SimpleParser[A] = sp { i =>
    p.run(i).mapError {
      case ParseError(h :: t) => ParseError((msg, h._2) :: t)
      case ParseError(Nil) => sys.error("Unexpected. Should be a non empty-list")
    }
  }

  override def scope[A](msg: String)(p: SimpleParser[A]): SimpleParser[A] = sp { i =>
    p.run(i).mapError { _.push(msg) }
  }

  override def errorLocation(e: ParseError): Location = e.stack.head._2
  override def errorMessage(e: ParseError): String = e.stack.head._1
}

