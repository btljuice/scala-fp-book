package sfpbook.ch12

import sfpbook.ch11.Functor

/**
 * All Applicative are Functor, because they can implement [[map]]. Primitives to implement, either:<br>
 * 1. [[unit]], [[map2]]<br>
 * 2. [[unit]], [[apply]]
 * @todo This definition allows to select the primitive set. I'd rather have this better typed where one would select
 *       to implement either Applicative1 or Applicative2, well defining which set of primitive was chosen.
 */
trait Applicative[F[_]] extends Functor[F] {
  // Primitives to implement.
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map2c(fa, fb)(f.curried)
  def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] = map2(ff, fa) { _(_) }

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(())) { (a, _) => f(a) } // Not final to allow Monad to override

  final def map3[A, B, C, D](a: F[A], b: F[B], c: F[C])(f: (A, B, C) => D): F[D] = map3c(a, b, c)(f.curried)
  final def map4[A, B, C, D, E](a: F[A], b: F[B], c: F[C], d: F[D])(f: (A, B, C, D) => E): F[E] = map4c(a, b, c, d)(f.curried)

  final def traverse[A, B](l: List[A])(f: A => F[B]): F[List[B]] = l.foldRight(unit(List.empty[B])) { (a, acc) => map2(f(a), acc) { _ :: _ } }
  final def sequence[A](l: List[F[A]]): F[List[A]] = traverse(l)(identity)
  final def replicateM[A](n: Int, fa: F[A]): F[List[A]] = (1 to n).foldLeft(unit(List.empty[A])) { (acc, _) => map2(fa, acc) { _ :: _} }
  final def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb) { (_ , _) }
  final def filterM[A](l: List[A])(f: A => F[Boolean]): F[List[A]] = l.foldRight(unit(List.empty[A])) { (a, acc) => map2(f(a), acc) { (p, l) => if(p) a :: l else l } }

  private[this] def map2c[A, B, C](a: F[A], b: F[B])(f: A => B => C): F[C] = apply(apply(unit(f))(a))(b)
  private[this] def map3c[A, B, C, D](a: F[A], b: F[B], c: F[C])(f: A => B => C => D): F[D] = apply(map2c(a, b)(f))(c)
  private[this] def map4c[A, B, C, D, E](a: F[A], b: F[B], c: F[C], d: F[D])(f: A => B => C => D => E): F[E] = apply(map3c(a, b, c)(f))(d)

  // Primitive alternative (unit, apply)
  private[this] def map2FromApply[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(a))(b)
}