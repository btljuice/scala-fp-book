package sfpbook.ch13

import scala.annotation.tailrec
import sfpbook.ch11.Monad
import sfpbook.ch7.Par
import sfpbook.ch7.Par.RichPar
import sfpbook.ch7.Par.Par

/**
 * F abstracts the suspension. Therefore we have the same implemenation whether we use TailRec or Async
 *
 * Also, Because of Return constructor and FlatMap constructor, Free has a monad for any F, thus why it is called
 * "free"
 */
sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.Return(f(a)))
}
object Free {
  case class Return[F[_], A](a : A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A] // Main difference between TailRec
  case class FlatMap[F[_], A, B](s: Free[F, A], k: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par.Par, A]

  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](m: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(m, f)
  }

  @tailrec def runTrampoline[A](a: TailRec[A]): A = step(a) match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(free, f) => free match {
      case Suspend(r) =>  runTrampoline(f(r()))
      case _ => sys.error("Unexpected; `step` eliminates these cases")
    }
  }

  def run[F[_], A](free: Free[F, A])(implicit m: Monad[F]): F[A] = step(free) match {
    case Return(a) => m.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => m.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Unexpected; `step` eliminates these cases")
    }
  }

  @tailrec private def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }
}
