package sfpbook.ch11

import sfpbook.ch6.State
import sfpbook.ch7.Par
import sfpbook.ch7.Par.Par
import sfpbook.ch8.Test.Gen

/** All monads are functor because they implement map (a abide to the same laws) */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]

  final def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))
  final def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma) { a => map(mb) { b => f(a, b) } }

  final def sequence[A](l: List[F[A]]): F[List[A]] = traverse(l)(identity)
  final def traverse[A,B](l: List[A])(f : A => F[B]): F[List[B]] = l match {
    case Nil => unit(Nil)
    case h :: t => map2(f(h), traverse(t)(f)) { _ :: _ }
  }
  final def replicateM[A](n: Int, m: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(m, replicateM(n-1, m)) { _ :: _ }
  final def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb){ (_, _) }
}

object Monad {
  object Instances {
    val genMonad = new Monad[Gen] {
      def unit[A](a: => A) = Gen.unit(a)
      def flatMap[A, B](m: Gen[A])(f: A => Gen[B]) = m flatMap f
    }
    val parMonad = new Monad[Par] {
      def unit[A](a: => A) = Par.unit(a)
      def flatMap[A, B](m: Par[A])(f: A => Par[B]) = m flatMap f
    }
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A) = Some(a)
      def flatMap[A, B](m: Option[A])(f: A => Option[B]) = m flatMap f
    }
    val streamMonad = new Monad[Stream] {
      def unit[A](a: => A) = Stream(a) // Not lazy over a
      def flatMap[A, B](m: Stream[A])(f: A => Stream[B]): Stream[B] = m flatMap f
    }
    val listMonad = new Monad[List] {
      def unit[A](a: => A) = a :: Nil
      def flatMap[A, B](m: List[A])(f: A => List[B]) =  m flatMap f
    }

    def stateMonad[T] = {
      type S[A] = State[T, A]
      new Monad[S] {
        def unit[A](a: => A) = State.value(a)
        def flatMap[A, B](m: S[A])(f: A => S[B]): S[B] = m flatMap f
      }
    }
  }
}
