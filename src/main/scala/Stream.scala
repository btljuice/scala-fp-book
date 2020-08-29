package sfpbook

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A]
  def toList: List[A]
  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]
  def foldRight[B](z: B)(f: (A, => B) => B): B

  final def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) =>
    if (p(a)) { Stream.cons(a, b) } else { Stream.empty[A] }
  }
  final def exists(p: A => Boolean): Boolean = foldRight(false) { (a, b) => p(a) || b }
  final def forall(p: A => Boolean): Boolean = foldRight(true) { (a, b) => p(a) && b }
}
case object Empty extends Stream[Nothing] {
  override def headOption = None
  override def toList = Nil
  override def take(n: Int) = Empty
  override def drop(n: Int) = Empty
  override def foldRight[B](z: B)(f: (A, => Nothing) => B) = z
}
case class SCons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {
  override def headOption = Some(head())
  override def toList = Cons(head(), tail().toList)
  override def take(n: Int) = if (n > 0) Stream.cons(head(), tail().take(n - 1)) else Stream.empty
  override def drop(n: Int) = if (n > 0) tail().drop(n - 1) else this
  override def foldRight[B](z: B)(f: (A, => B) => B): B = f(head(), tail().foldRight(z)(f))
}

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    SCons(() => h, () => t)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
