package sfpbook.ch9

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
  val noop: Parser[String] // Should return the string unparsed
  def alwaysFail[A]: Parser[A]
  final def empty: Parser[Boolean] = noop.map(_.isEmpty)
  def nonEmpty: Parser[Boolean] = noop.map(_.nonEmpty)
  final def succeed[A](a: => A): Parser[A] = noop.map(_ => a)

  final def count0(a: Char): Parser[Int] = char(a).many.map(_.size)
  final def count1(a: Char): Parser[Int] = char(a).many1.map(_.size)

  implicit def char(a: Char): Parser[Char] = string(a.toString).map(_.head)
  implicit def string(s: String): Parser[String]

  // Primitives to implement
  // - "and" operator: For an "and" parser, we'd have to decide for which of the
  //   2 successful runs, we retain the result. An arbitrary choice could be made. (1st one)

  // Let's say we decide to retain the first result, then this operation is not commutative
  // The operation will be associative
  protected[this] def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  protected[this] def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  protected[this] def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  protected[this] def many[A](p: Parser[A], from: Int, to: Option[Int]): Parser[List[A]]
  protected[this] def concatenate[A](ps: Seq[Parser[A]]): Parser[List[A]]
  protected[this] def join[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)]

  // Operators for parser's expressiveness
  implicit class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: => Parser[B]): Parser[B] = or(p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = many(0)
    def many(from: Int, to: Int): Parser[List[A]] = many(from, Some(to))
    def many(from: Int, to: Option[Int] = None): Parser[List[A]] = self.many(p, from, to)
    def many1: Parser[List[A]] = many(1)
    def many1(to: Int): Parser[List[A]] = many(1, to)
    def many1(to: Option[Int]): Parser[List[A]] = many(1, to)

    def exactly(n: Int): Parser[List[A]] = many(n, n)
  }
}
