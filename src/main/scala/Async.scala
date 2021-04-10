package sfpbook.ch13

import scala.annotation.tailrec
import sfpbook.ch7.Par
import sfpbook.ch7.Par.RichPar
import sfpbook.ch7.Par.Par

sealed trait Async[A] {
  def flatMap[B](f: A => Async[B]): Async[B] = Async.FlatMap(this, f)
  def map[B](f: A => B): Async[B] = flatMap(a => Async.Return(f(a)))
}
object Async {
  case class Return[A](a : A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A] // Main difference between TailRec
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  @tailrec def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => r.flatMap(a => run(f(a)))
      case _ => sys.error("Unexpected; `step` eliminates these cases")
    }
  }
}
