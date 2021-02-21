package sfpbook.ch9

// Goal design a Parser combinator through "algebraic design".
// "Algebraic design" means thinking about the laws that should hold before implementation

// - String as input
// - What's the input format? (HTML, JSON, [a-z])




// Parser[+_] means the `Parser` is a type constructor

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  val identity: Parser[String]
  def alwaysFail[A]: Parser[A]
  def zeroOrMore(a: Char): Parser[Int]
  def oneOrMore(a: Char): Parser[Int]

  implicit def char(a: Char): Parser[Char]
  implicit def string(s: String): Parser[String]

  protected[this] def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  protected[this] def and[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  protected[this] def concat[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  implicit class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def and[B >: A](p2: => Parser[B]): Parser[B] = self.and(p, p2)
    def &[B >: A](p2: => Parser[B]): Parser[B] = self.and(p, p2)
    def +[B >: A](p2: => Parser[B]): Parser[B] = self.concat(p, p2)
    def concat[B >: A](p2: => Parser[B]): Parser[B] = self.concat(p, p2)
  }
}