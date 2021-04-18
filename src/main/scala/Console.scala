package sfpbook.ch13

import scala.io.StdIn
import scala.util.Try
import sfpbook.ch7.Par
import sfpbook.ch7.Par.Par

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

object Console {
  object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] = Try { StdIn.readLine() }.toOption
  }
  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
  }
}
