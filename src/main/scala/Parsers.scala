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
  val empty: Parser[Boolean] = noop.map(_.isEmpty)
  val nonEmpty: Parser[Boolean] = noop.map(_.nonEmpty)
  def alwaysFail[A]: Parser[A]
  final def succeed[A](a: => A): Parser[A] = noop.map(_ => a)

  final def count0(a: Char): Parser[Int] = char(a).many.slice.map(_.length)
  final def count1(a: Char): Parser[Int] = char(a).many1.slice.map(_.length)

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
  protected[this] def slice[A](pa: Parser[A]): Parser[String]
  protected[this] def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)]

  private[this] def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
    pa ** pb map { case (a, b) => f(a, b) }

  private[this] final def concatenate[A](ps: Seq[Parser[A]]): Parser[List[A]] =
    if (ps.isEmpty) succeed(Nil)
    else ps.head + concatenate(ps.tail)

  private[this] final def fill[A](n: Int)(p: Parser[A]): Parser[List[A]] = concatenate(List.fill(n)(p))

  private[this] final def many0[A](p: Parser[A], to: Option[Int] = None): Parser[List[A]] =
    if (to.exists(_ <= 0)) succeed(Nil)
    else many1(p, to) | succeed(Nil)

  private[this] final def many1[A](p: Parser[A], to: Option[Int] = None): Parser[List[A]] = {
    require(to.forall(_ >= 1))
    if (to.exists(_ <= 1)) fill(1)(p)
    else p + many0(p, to.map(_-1))
  }

  private[this] final def many[A](p: Parser[A], from: Int, to: Option[Int]): Parser[List[A]] = {
    require(from >= 0, s"invalid range: from($from) must be > 0")
    require(to.forall(_ >= from), s"invalid range: to($to) must be <= from($from)")
    fill(from)(p) ** many0(p, to.map(_-from)) map { case (l1, l2) => l1 ::: l2 }
  }


  // Operators for parser's expressiveness
  implicit class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: => Parser[B]): Parser[B] = or(p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def +[B >: A](p2: Parser[List[B]]): Parser[List[B]] = p ** p2 map { case (h, t) => h :: t }

    def map2[B, C](pb: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, pb)(f)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice: Parser[String] = self.slice(p)

    def many: Parser[List[A]] = many(0)
    def many(from: Int, to: Int): Parser[List[A]] = many(from, Some(to))
    def many(from: Int, to: Option[Int] = None): Parser[List[A]] = self.many(p, from, to)
    def many1: Parser[List[A]] = many(1)
    def many1(to: Int): Parser[List[A]] = many(1, to)
    def many1(to: Option[Int]): Parser[List[A]] = many(1, to)

    def exactly(n: Int): Parser[List[A]] = many(n, n)
  }
}
