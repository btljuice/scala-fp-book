package sfpbook.ch11

import sfpbook.ch6.State
import sfpbook.ch7.Par
import sfpbook.ch7.Par.Par
import sfpbook.ch8.Test.Gen

/** All monads are functor because they implement map (a abide to the same laws) */
trait Monad[F[_]] extends Functor[F] { self =>
  def unit[A](a: => A): F[A]
  def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]

  final def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))
  final def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma) { a => map(mb) { b => f(a, b) } }

  final def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
  final def sequence[A](l: List[F[A]]): F[List[A]] = traverse(l)(identity)
  final def traverse[A,B](l: List[A])(f : A => F[B]): F[List[B]] =
    l.foldRight(unit(List.empty[B])) { (a, acc) => map2(f(a), acc) { _ :: _ } }

  final def replicateM[A](n: Int, m: F[A]): F[List[A]] =
    (1 to n).foldLeft(unit(List.empty[A])) { (acc, _) => map2(m, acc) { _ :: _ } }

  final def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb){ (_, _) }
  final def filterM[A](l: List[A])(f: A => F[Boolean]): F[List[A]] =
    l.foldRight(unit(List.empty[A])) { (a, acc) => map2(f(a), acc) { (p, l) => if(p) a :: l else l } }
  final def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  ///// Primitives alternatives:
  // 1. unit, compose
  private[this] def flatMapFromCompose[A, B](m: F[A])(f: A => F[B]): F[B] = compose[Unit, A, B](_ => m, f)(())
  // 2. unit, map, join
  private[this] def flatMapFromMapAndJoin[A, B](m: F[A])(f: A => F[B]): F[B] = join(map(m)(f))

  implicit class MonadOps[A](m: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = self.flatMap(m)(f)
    def map[B](f: A => B): F[B] = self.map(m)(f)
    def map2[B, C](mb: F[B])(f: (A, B) => C) = self.map2(m, mb)(f)
    def **[B](mb: F[B]): F[(A, B)] = self.product(m, mb)
  }
  implicit class MonadOps2[A](m: F[F[A]]) {
    def join: F[A] = self.join(m)
  }
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
      new Monad[({type f[x] = State[T, x]})#f] {
        override def unit[A](a: => A): State[T, A] = State.value(a)
        override def flatMap[A, B](m: State[T, A])(f: A => State[T, B]): State[T, B] = m flatMap f
      }
    }

    // This is probably the simplest monad that can be. It's just a container over value of A
    case class Id[A](value: A)
    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = Id(a)
      def flatMap[A, B](m: Id[A])(f: A => Id[B]): Id[B] = f(m.value)
    }
  }
}