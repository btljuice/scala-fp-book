package sfpbook.ch12

import sfpbook.ch11.Functor

/**
 * All Applicative are Functor, because they can implement [[map]].
 * Primitives sets are:<br>
 * <b>1. [[unit]], [[map2]] (selected) </b><br>
 * 2. [[unit]], [[apply]]<br>
 * @note If the interface was to allow the choice of the primitive set, I would do it through a distinct types, to not
 *       allow for any ambiguity.
 */
trait Applicative[F[_]] extends Functor[F] {
  //// Primitives
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(())) { (a, _) => f(a) } // Not final to allow Monad to override

  final def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] = map2(ff, fa) { _(_) }

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

object Applicative {
  object Instances {
    val streamZipApplicative = new Applicative[Stream] {
      def unit[A](a: => A) = Stream.continually(a)
      def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C) = fa zip fb map f.tupled
    }

    def validationApplicative[E] = new Applicative[({type a[x] = Validation[E, x]})#a] {
      import Validation._
      def unit[A](a: => A) = Success(a)
      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) = (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a,b))
          case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++: hb +: tb)
          case (f: Failure[E], _) => f
          case (_, f: Failure[E]) => f
        }
    }
  }
}
