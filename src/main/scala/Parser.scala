package sfpbook.ch9

// Goal design a Parser combinator through "algebraic design".
// "Algebraic design" means thinking about the laws that should hold before implementation

// - String as input
// - What's the input format? (HTML, JSON, [a-z])




// Parser[+_] means the `Parser` is a type constructor
trait Parsers[ParseError, Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(a: Char): Parser[Char]
  def string(s: String): Parser[String]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
}
