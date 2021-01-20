package sfpbook

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._
  def isEmpty: Boolean
  final def nonEmpty: Boolean = !isEmpty
  final def exists(p: A => Boolean): Boolean = foldRight(false) { p(_) || _ }
  final def forall(p: A => Boolean): Boolean = foldRight(true) { p(_) && _ }
  final def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).foldRight(true) {
    case ((Some(a0), Some(a1)), z) => a0 == a1 && z
    case ((None, Some(_)), _) => false
    case ((_, None), _) => true
  }
  final def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails.exists(_.startsWith(s))

  final def headOption: Option[A] = foldRight[Option[A]](None) { (a, _) => Some(a) }
  final def head: A = headOption.get
  final def find(p: A => Boolean): Option[A] = filter(p).headOption
  def take(n: Int): Stream[A]
  final def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, s) => if (p(a)) cons(a, s) else Empty }

  final def foldRight[B](z: => B)(f: (=> A, => B) => B): B = this match {
    case Empty => z
    case SCons(h, t) => f(h(), t().foldRight(z)(f))
  }
  final def foldLeft[B](z: => B)(f: (=> B, => A) => B): B = {
    @tailrec def impl(z: => B, s: Stream[A]): B = s match {
      case Empty => z
      case SCons(h, t) => impl( f(z, h()), t() )
    }
    impl(z, this)
  }

  final def reverse: Stream[A] = foldLeft(empty[A]) { (s, a) => cons(a, s) }

  final def map[B](f: A => B): Stream[B] = foldRight(empty[B]) { (a, s) => cons(f(a), s) }
  final def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) { (a, s) => f(a).append(s) }
  final def filter(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, s) => if (p(a)) cons(a, s) else s }
  final def filterNot(p: A => Boolean): Stream[A] = filter(!p(_))
  final def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(cons)

  final def zip[B](s: Stream[B]): Stream[(A, B)] = Stream.unfoldZipWith(this, s)
  final def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfoldZipAll(this, s)

  final def tails: Stream[Stream[A]] = unfold(this) { case s@SCons(_, t) => Some(s -> t()); case Empty => None }
  final def scanRight[B](z: => B)(f: (=> A, => B) => B): Stream[B] = foldRight(cons(z, empty[B])) { (a, sb) => cons(f(a, sb.head), sb) }
  final def toList: List[A] = foldRight[List[A]](Nil) { (a, l) => a :: l }
}

object Empty extends Stream[Nothing] {
  override def isEmpty = true
  override def take(n: Int): Stream[Nothing] = Empty
}
final case class SCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  import Stream._
  override def isEmpty: Boolean = false
  override def take(n: Int): Stream[A] = if (n > 0) cons(h(), t().take(n-1)) else Empty
}

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h // Necessary so it's only eval'd once
    lazy val tail = t // Necessary so it's only eval'd once
    SCons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  // Infinite streams
  lazy val zeros: Stream[Int] = constant(0)
  lazy val ones: Stream[Int] = constant(1)
  lazy val fibs: Stream[Int] = unfold((0, 1)) { case (n0, n1) => Some((n0, (n1, n0+n1))) }

  def from(n: Int): Stream[Int] = unfold(n) { n => Some((n, n+1)) }
  def constant[A](a: A): Stream[A] = unfold(()) { _ => Some(a -> ()) }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) map { case (a,s) => cons(a, unfold(s)(f)) } getOrElse { empty }


  def unfoldMap[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) { case Empty => None; case SCons(h, t) => Some(f(h()) -> t()) }
  def unfoldTake[A](s: Stream[A])(n: Int): Stream[A] = unfold((n, s)) {
    case (n, SCons(h, t)) if n > 0 => Some(h() -> (n - 1, t()))
    case _ => None
  }
  def unfoldTakeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = unfold(s) {
    case SCons(h , t) if p(h()) => Some( h() -> t() )
    case _ => None
  }
  def unfoldZipWith[A, B](sa: Stream[A], sb: Stream[B]): Stream[(A, B)] = unfold(sa -> sb) {
    case (SCons(h1, t1), SCons(h2, t2)) => Some{ (h1(), h2()) -> (t1(), t2()) }
    case _ => None
  }
  def unfoldZipAll[A, B](sa: Stream[A], sb: Stream[B]): Stream[(Option[A], Option[B])] = unfold(sa -> sb) {
    case (SCons(h1, t1), SCons(h2, t2)) => Some{ (Some(h1()), Some(h2())) -> (t1(), t2()) }
    case (SCons(h1, t1), Empty) => Some{ (Some(h1()), None) -> (t1(), Empty) }
    case (Empty, SCons(h2, t2)) => Some{ (None, Some(h2())) -> (Empty, t2()) }
    case _ => None
  }
}
