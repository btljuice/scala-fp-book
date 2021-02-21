package sfpbook.ch9

// Goal design a Parser combinator through "algebraic design".
// "Algebraic design" means thinking about the laws that should hold before implementation

// - String as input
// - What's the input format? (HTML, JSON, [a-z])




// Parser[+_] means the `Parser` is a type constructor

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  implicit def char(a: Char): Parser[Char]
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a : A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

  }
}
