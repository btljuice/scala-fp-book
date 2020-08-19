package sfpbook

import scala.collection.AbstractSeq

sealed trait Tree[+T] {
  final def length: Int = fold(_ => 1)(1 + _ + _)
  final def depth: Int = fold(_ => 1)(Math.max(_, _) + 1)
  final def map[A](f: T => A): Tree[A] = fold[Tree[A]]{ v => Leaf(f(v)) }{ (l, r) => Branch(l, r)}
  def fold[A](f: T => A)(m: (A, A) => A): A
}
case class Leaf[T](value: T) extends Tree[T] {
  override def fold[A](f: T => A)(m: (A, A) => A) = f(value)
}
case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def fold[A](f: T => A)(m: (A, A) => A) = m(left.fold(f)(m), right.fold(f)(m))
}
