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
sealed trait TailRec[A] {
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = TailRec.FlatMap(this, f)
  def map[B](f: A => B): TailRec[B] = flatMap(a => TailRec.Return(f(a)))

  final def **[B](io: TailRec[B]): TailRec[(A, B)] = TailRec.monad.product(this, io)
}

object TailRec {
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](io: TailRec[A], k: A => TailRec[B]) extends TailRec[B] // k as in Continuation

  // Scala fp book p.239. FlatMap is analog to a coroutine
  @tailrec def run[A](io: TailRec[A]): A = io match {
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

  def apply[A](f: => A): TailRec[A] = Suspend(() => f)
  def unit[A](a: => A): TailRec[A] = TailRec(a)
  def value[A](a: A): TailRec[A] = Return(a)
  val empty: TailRec[Unit] = Return(())

  def printLine(msg: String): TailRec[Unit] = TailRec { println(msg) }
  def readLine: TailRec[String] = TailRec { StdIn.readLine }

  val monoid: Monoid[TailRec[Unit]] = new Monoid[TailRec[Unit]] {
    override def op(a1: TailRec[Unit], a2: TailRec[Unit]): TailRec[Unit] = a1.flatMap(_ => a2)
    override def zero: TailRec[Unit] = empty
  }

  val monad: Monad[TailRec] = new Monad[TailRec] {
    override def unit[A](a: => A): TailRec[A] = TailRec(a)
    override def flatMap[A, B](m: TailRec[A])(f: A => TailRec[B]): TailRec[B] = m flatMap f
  }
}
