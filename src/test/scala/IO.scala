package sfpbook.ch13

import scala.annotation.tailrec
import scala.io.StdIn
import sfpbook.ch10.Monoid
import sfpbook.ch11.Monad

/** Benefits of IO
 *  - Provides clear separation from pure VS impure code
 *  - IO computations can be manipulated as ordinary values. e.g. they can be sotred into list, combined, created
 *       dynamically and so on.
 *  - IO can abstract whether it's a sync or async io, etc.
 */
sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(a => IO.Return(f(a)))

  final def **[B](io: IO[B]): IO[(A, B)] = IO.monad.product(this, io)
}

object IO {
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](io: IO[A], k: A => IO[B]) extends IO[B] // k as in Continuation

  // Scala fp book p.239. FlatMap is analog to a coroutine
  @tailrec def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      // ***** Neat trick! *******
      // Uses the "IO Monad associativity law" to right associate and retrieve the first chained IO
      // EXAMPLE: io0.flatMap(f1).flatMap(f2).flatMap(f3).
      // 1. Is constructed as :
      //    FlatMap(FlatMap(FlatMap(io, f1), f2), f3)
      // 2. When run is executed, it is eventually reconstructed as
      //    FlatMap(io0, a => FlatMap(f1(a), b => FlatMap(f2(b), f3)))
      case FlatMap(y, g) => run( y.flatMap(b => g(b) flatMap f) )

    }
  }

  def apply[A](f: => A): IO[A] = Suspend(() => f)
  def unit[A](a: => A): IO[A] = IO(a)
  def value[A](a: A): IO[A] = Return(a)
  val empty: IO[Unit] = Return(())

  def printLine(msg: String): IO[Unit] = IO { println(msg) }
  def readLine: IO[String] = IO { StdIn.readLine }

  val monoid: Monoid[IO[Unit]] = new Monoid[IO[Unit]] {
    override def op(a1: IO[Unit], a2: IO[Unit]): IO[Unit] = a1.flatMap(_ => a2)
    override def zero: IO[Unit] = empty
  }

  val monad: Monad[IO] = new Monad[IO] {
    override def unit[A](a: => A): IO[A] = IO(a)
    override def flatMap[A, B](m: IO[A])(f: A => IO[B]): IO[B] = m flatMap f
  }
}
