package sfpbook.ch11

import sfpbook.ch7.Par
import sfpbook.ch7.Par.Par
import sfpbook.ch8.Test.Gen

/** All monads are functor because they implement map (a abide to the same laws) */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]

  final def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))
  final def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma) { a => map(mb) { b => f(a, b) } }
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
  }
}
