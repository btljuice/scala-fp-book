package sfpbook.ch13

import sfpbook.ch13.{TailRec => IO}

import scala.annotation.tailrec

// - It is always possible to factor an impure procedure into a pure "core" function, and 2 procedures w/ side effect:
//   1. one side effects that supplies input
//   2. one pure function to process it
//   3. one side effect to execute outputs upon the pure function result
// Example below

case class Player(name: String, score: Int)

object PlayerExample {
  def impureContest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score) println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score) println(s"${p2.name} is the winner!")
    else println("It's a draw.")

  // To pure functions
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p.fold("It's a draw.") { p => s"${p.name} is the winner!" }

  def separatedContest(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))

  // Contest is now pure, as it simply returns an IO instance
  def ioContest(p1: Player, p2: Player): IO[Unit] = IO.printLine(winnerMsg(winner(p1, p2)))
}

object TemperatureExample {
  def farenheitToCelius(f: Double): Double = (f-32)*5/9

  def converter: IO[Double] = for {
    _ <- IO.printLine("Enter a temperature in Farenheit degrees: ")
    f <- IO.readLine
    c = farenheitToCelius(f.toDouble)
    _ <- IO.printLine(s"Celcius temperature = $c")
  } yield ()
}

object OtherExample {
  val echo: IO[Unit] = IO.readLine.flatMap(IO.printLine)
  val readInt: IO[Int] = IO.readLine.map(_.toInt)
  def readInts(n: Int): IO[List[Int]] = IO.monad.replicateM(n, readInt)
}

object FactorialRepl {
  def apply: IO[Unit] = {
    def loop: IO[Unit] = for {
      maybeInt <- IO.readLine.map { case "q" => None; case s => Some(s.toInt) }
      f = maybeInt.map(i => (1 to i).product) getOrElse sys.error("Crash spectacularly")
      _ <- IO.printLine(s"factorial: $f")
      _ <- loop // TODO: This should be tail recursive
    } yield ()

    for {
      _ <- IO.printLine("""The Amazing Factorial REPL, v2.0
          | q - quit
          | <number> - compute the factorial of the given number
          | <anything else> - crash spectacularly
          |""".stripMargin)
      _ <- loop
    } yield ()
  }
}
