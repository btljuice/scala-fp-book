package sfpbook

import annotation.tailrec

sealed trait List[+T] {
  def tail: List[T]
  final def isEmpty: Boolean = this match { case Nil => true; case _ => false }
  final def nonEmpty: Boolean = !isEmpty
  final def length: Int = foldLeft(0) { case (n, _) => n + 1 }

  @tailrec final def forall(p: T => Boolean): Boolean = this match {
    case Cons(h, t) => p(h) && t.forall(p)
    case Nil => true
  }
  @tailrec final def exists(p: T => Boolean): Boolean = this match {
    case Cons(h, t) => p(h) || t.exists(p)
    case Nil => false
  }

  @tailrec final def drop(n: Int): List[T] = this match {
    case Cons(_, t) if n > 0 => t.drop(n-1)
    case l => l
  }
  @tailrec final def dropWhile(p: T => Boolean): List[T] = this match {
    case Cons(h, t) if p(h) => t.dropWhile(p)
    case l => l
  }
  final def take(n: Int): List[T] = this match {
    case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
    case _ => Nil
  }
  final def takeWhile(p: T => Boolean): List[T] = this match {
    case Cons(h, t) if p(h) => Cons(h, t.takeWhile(p))
    case _ => Nil
  }

  final def ::[A >: T](prefix: A): List[A] = Cons(prefix, this)
  final def :::[A >: T](l: List[A]): List[A] = l.foldRight(this: List[A]) { (h, l) => Cons(h, l) }

  @tailrec final def foldLeft[A](z: A)(f: (A, T) => A): A = this match {
    case Cons(h, t) => t.foldLeft(f(z, h))(f)
    case Nil => z
  }
  final def foldRight[A](z: A)(f: (T, A) => A): A = reverse.foldLeft(z)((a, t) => f(t, a))

  final def reverse: List[T] = foldLeft(List.empty[T]){ (l, h) => Cons(h, l) }
  final def map[A](f: T => A): List[A] = foldRight(List.empty[A]){ (t, l) => Cons(f(t), l) }
  final def filter(f: T => Boolean): List[T] = flatMap { t => if (f(t)) Cons(t, Nil) else Nil }
  final def flatMap[A](f: T => List[A]): List[A] = foldRight(List.empty[A]){ (t, l) => f(t) ::: l }
  final def zipWith[A, B](l: List[A])(f: (T, A) => B): List[B] = (this, l) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), t1.zipWith(t2)(f))
    case (_, Nil) | (Nil, _) => Nil
  }
}

case object Nil extends List[Nothing] { override def tail = Nil }
final case class Cons[+T](head: T, tail: List[T]) extends List[T]

object List {
  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  def empty[A]: List[A] = Nil
}
