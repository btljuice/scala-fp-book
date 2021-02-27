package sfpbook.ch9

case class SimpleParser[+A]() {
  import SimpleParser._
  def run(input: String): Result[A] = ???
}

object SimpleParser {
  sealed trait Result[+A]
  object Result {
    case class Success[+A](get: A, location: Location) extends Result[A]
    case class Failure(error: ParseError) extends Result[Nothing]
  }

  case class ParseError(stack: List[(Location, String)])
}

