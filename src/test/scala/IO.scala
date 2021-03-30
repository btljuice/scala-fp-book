package sfpbook.ch13

import scala.io.StdIn
import sfpbook.ch10.Monoid
import sfpbook.ch11.Monad

trait IO[A] {
  def run: A
  def map[B](f: A => B): IO[B] = IO { f(run) }
  def flatMap[B](f: A => IO[B]): IO[B] = IO { f(run).run }

  final def **[B](io: IO[B]): IO[(A, B)] = IO.monad.product(this, io)
}

object IO {
  def apply[A](f: => A): IO[A] = new IO[A] { override def run: A = f }
  def unit[A](a: => A): IO[A] = IO(a)
  val empty: IO[Unit] = IO { () }

  def printLine(msg: String): IO[Unit] = IO { println(msg) }
  def readLine: IO[String] = IO { StdIn.readLine }

  val monoid: Monoid[IO[Unit]] = new Monoid[IO[Unit]] {
    override def op(a1: IO[Unit], a2: IO[Unit]): IO[Unit] = a1 ++ a2
    override def zero: IO[Unit] = empty
  }

  val monad: Monad[IO] = new Monad[IO] {
    override def unit[A](a: => A): IO[A] = IO(a)
    override def flatMap[A, B](m: IO[A])(f: A => IO[B]): IO[B] = m flatMap f
  }
}
