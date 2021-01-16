package sfpbook

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def isEmpty: Boolean
  def toList: List[A]
  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]
  def dropWhile(p: A => Boolean): Stream[A]
  def foldRight[B](z: B)(f: (A, => B) => B): B

  final def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) => if (p(a)) { Stream.cons(a, b) } else { Stream.empty } }
  final def exists(p: A => Boolean): Boolean = foldRight(false) { p(_) || _ }
  final def forall(p: A => Boolean): Boolean = foldRight(true) { p(_) && _ }
  final def headOption: Option[A] = foldRight(None: Option[A]){ (a, _) => Some(a) }
  final def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]){ (a, b) => Stream.cons(f(a), b) }
  final def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A]){ (a, b) => if (f(a)) Stream.cons(a, b) else b }
  final def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s){ Stream.cons(_, _) }
  final def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B]){ (a, b) => f(a).append(b) }
  final def find(p: A => Boolean): Option[A] = filter(p).headOption
  final def startsWith[B >: A](s: Stream[B]): Boolean = Stream.unfoldZipWith(this, s).forall{ z => z._1 == z._2 }
  final def tails: Stream[Stream[A]] = Stream.unfold(this) { case Empty => None; case s@SCons(_, t) => Some((s, t())) }
  final def hasSubSequence[B >: A](sub: Stream[B]): Boolean = tails.exists(_.startsWith(sub))
  final def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream.cons(z, Stream.empty)){ (a, sb) => Stream.cons(f(a, sb.headOption.get), sb) }
}

case object Empty extends Stream[Nothing] {
  override def isEmpty = true
  override def toList = Nil
  override def take(n: Int) = Empty
  override def drop(n: Int) = Empty
  override def dropWhile(p: Nothing => Boolean) = Empty
  override def foldRight[B](z: B)(f: (Nothing, => B) => B) = z

}
case class SCons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {
  override def isEmpty = false
  override def toList = Cons(head(), tail().toList)
  override def take(n: Int) = if (n > 0) Stream.cons(head(), tail().take(n - 1)) else Stream.empty
  override def drop(n: Int) = if (n > 0) tail().drop(n - 1) else this
  override def dropWhile(p: A => Boolean) = if (p(head())) tail().dropWhile(p) else this
  override def foldRight[B](z: B)(f: (A, => B) => B) = f(head(), tail().foldRight(z)(f))
}

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    SCons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map { case (a, s) => Stream.cons(a, unfold(s)(f)) }.getOrElse(empty)
  def constant[A](a: => A): Stream[A] = unfold(()) { _ => Some((a, ())) }
  def from(n: Int): Stream[Int] = unfold(n) { n => Some(n, n + 1) }
  def fibs: Stream[Int] = unfold((0, 1)) { case (i, j) => Some((i, (j, i + j))) }
  val ones: Stream[Int] = constant(1)

  def unfoldMap[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
    case Empty => None
    case SCons(head, tail) => Some((f(head()), tail()))
  }
  def unfoldTake[A](s: Stream[A])(n: Int): Stream[A] = unfold((s, n)) {
    case (Empty, _) => None
    case (_, i) if i <= 0 => None
    case (SCons(h, t), i) => Some(h(), (t(), i - 1))
  }
  def unfoldTakeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = unfold(s) {
    case Empty => None
    case SCons(h, t) => if (p(h())) Some((h(), t())) else None
  }
  def unfoldZipWith[A, B](sa: Stream[A], sb: Stream[B]): Stream[(A, B)] = unfold{(sa, sb)}{
    case (Empty, _) | (_, Empty) => None
    case (SCons(h1, t1), SCons(h2, t2)) => Some((h1(), h2()),(t1(), t2()))
  }
  def unfoldZipAll[A, B](sa: Stream[A], sb: Stream[B]): Stream[(Option[A], Option[B])] = unfold{(sa, sb)}{
    case (Empty, Empty) => None
    case (Empty, SCons(h2, t2)) => Some((None, Some(h2())),(Empty, t2()))
    case (SCons(h1, t1), Empty) => Some((Some(h1()), None),(t1(), Empty))
    case (SCons(h1, t1), SCons(h2, t2)) => Some((Some(h1()), Some(h2())),(t1(), t2()))
  }
}
