package sfpbook.ch9

import scala.util.matching.Regex

// - Goal design a Parser combinator through "algebraic design".
// - "Algebraic design" means thinking about the laws that should hold before implementation
// - String as input
// - What's the input format? (HTML, JSON, [a-z])

// Parser[+_] means the `Parser` is a type constructor.
// ANSME: I'm not sure yet how the covariant. Does it mean Parser implementation has
//        to be covariant on its parameter. We'll figure it out pretty soon.
trait Parsers[ParseError, Parser[+_]] { self =>
  // Executes the parser
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Some default parsers. Not sure yet if they are all relevant
  val noop: Parser[String] // Returns the string unparsed
  def alwaysFail[A]: Parser[A]
  def succeed[A](a: => A): Parser[A]

  final def count0(a: Char): Parser[Int] = char(a).many.slice.map(_.length)
  final def count1(a: Char): Parser[Int] = char(a).many1.slice.map(_.length)

  implicit def char(a: Char): Parser[Char] = string(a.toString).map(_.head)
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  // Primitives to implement
  protected[this] def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  protected[this] def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  protected[this] def slice[A](pa: Parser[A]): Parser[String]

  private[this] final def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p) { a => succeed(f(a)) }
  private[this] final def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = map2(pa, pb)(_ -> _)
  private[this] final def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] = for { a <- pa; b <- pb } yield f(a, b)

  private[this] final def concatenate[A](ps: Seq[Parser[A]]): Parser[List[A]] =
    if (ps.isEmpty) succeed(Nil)
    else ps.head + concatenate(ps.tail)

  private[this] final def fill[A](n: Int)(p: Parser[A]): Parser[List[A]] = concatenate(List.fill(n)(p))

  private[this] final def many[A](p: Parser[A], from: Int, to: Option[Int]): Parser[List[A]] = {
    require(from >= 0, s"invalid range: from($from) must be > 0")
    require(to.forall(_ >= from), s"invalid range: to($to) must be <= from($from)")
    if (from > 0) fill(from)(p) ** many(p, 0, to.map(_-from)) map { case (l1, l2) => l1 ::: l2 }
    else if (to.exists(_ <= 0)) succeed(Nil)
    else many(p, 1, to.map(_-1)) | succeed(Nil)
  }


  // Operators for parser's expressiveness
  implicit class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: => Parser[B]): Parser[B] = or(p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def +[B >: A](p2: => Parser[List[B]]): Parser[List[B]] = p ** p2 map { case (h, t) => h :: t }

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, pb)(f)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice: Parser[String] = self.slice(p)

    def many(from: Int, to: Option[Int] = None): Parser[List[A]] = self.many(p, from, to)
    def many(from: Int, to: Int): Parser[List[A]] = many(from, Some(to))
    def many: Parser[List[A]] = many(0)
    def many1(to: Option[Int]): Parser[List[A]] = many(1, to)
    def many1(to: Int): Parser[List[A]] = many(1, to)
    def many1: Parser[List[A]] = many(1)

    def exactly(n: Int): Parser[List[A]] = many(n, n)
  }
}
