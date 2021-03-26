package sfpbook.ch11

import sfpbook.ch12.Applicative
import sfpbook.ch6.State
import sfpbook.ch7.Par
import sfpbook.ch7.Par.Par
import sfpbook.ch8.Test.Gen

/**
 * All monads are functor because they implement map (a abide to the same laws).<br>
 * All monads are applicative because they can implement map2.<br>
 * Possible primitives:<br>
 *     <b>1. unit, flatMap (selected</b><br>
 *     2. unit, compose<br>
 *     3. unit, map, join
 * @note If the interface was to allow the choice of the primitive set, I would do it through a distinct types, to not
 *       allow for any ambiguity.
 *
 * @note
 *    - flatMap is the first combinator that can go from `F[F[A]]` to `F[A]`
 *    - It's also the first combinator that provides a way to define a new F[B] dependent of the result of A. (e.g.
 *      build F[B] from the result of A). You can't do that w/ map or map2.
 *    - Priorhand, (w/ Functor or Applicative), it's only possible to go from F[A] => F[B] of F[A] => `F[F[B]]` but not
 *     `F[F[A]]` => F[B]<br>
 *    - "With Applicative, the structure of our computation is fixed." - FP with scala
 *    - "With Monad, the results of previous computations may influence what computations to run next." - FP in scala
 *
 * In short (TL;DR):<br>
 * 1. Functor.unit is the basis to start <i>creating</i> a F[_]<br>
 * 2. Functor.map provides a way to <i>modify</i> A within F[A], but not changing F<br>
 * 3. Applicative.map2 provides a way to <i>combine</i> multiple independent F[A], F[B], ... to a F[C]<br>
 * 4. flatMap provides a way to build F[B] <i>dependent</i> from a prior F[A].<br>
 *    Also provides way of <i>collapsing</i> `F[F[F[...[A]]]]` to F[A]
 *
 * Type constructors like: Par, Option, List, Parser, Gen are often call "effects".
 * We sometimes use the word "monadic effects", "applicative effects".
 * "effects" is used in contrast to "side-effects", because the latter violates referential transparencies.
 *
 * " Applicative computations have fixed structure and simply sequence effects whereas monadic computations may choose
 *   structure dynamically, based on previous effects." - FP in scala
 * " Example:
 *   - A context-sensitive grammar can be implemented by a monadic parser.
 *   - An applicative parser can only implement a context-free grammar." - FP in scala
 */
trait Monad[F[_]] extends Applicative[F] { self =>
  override def unit[A](a: => A): F[A]
  def flatMap[A, B](m: F[A])(f: A => F[B]): F[B]

  final override def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))
  final override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma) { a => map(mb) { b => f(a, b) } }

  final def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
  final def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  implicit class MonadOps[A](m: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = self.flatMap(m)(f)
    def map[B](f: A => B): F[B] = self.map(m)(f)
    def map2[B, C](mb: F[B])(f: (A, B) => C) = self.map2(m, mb)(f)
    def **[B](mb: F[B]): F[(A, B)] = self.product(m, mb)
  }
  implicit class MonadOps2[A](m: F[F[A]]) {
    def join: F[A] = self.join(m)
  }

  ///// Primitives alternatives:
  // 1. unit, compose
  private[this] def flatMapFromCompose[A, B](m: F[A])(f: A => F[B]): F[B] = compose[Unit, A, B](_ => m, f)(())
  // 2. unit, map, join
  private[this] def flatMapFromMapAndJoin[A, B](m: F[A])(f: A => F[B]): F[B] = join(map(m)(f))
}

object Monad {
  // Ex. 12.11
//  def compose[F[_], G[_]](f: Monad[F], g: Monad[G]): Monad[({type h[x] = F[G[x]]})#h] =
//    new Monad[({type h[x] = F[G[x]]})#h] {
//      override def unit[A](a: => A): F[G[A]] = f.unit(g.unit(a))
//      override def flatMap[A, B](m: F[G[A]])(h: A => F[G[B]]): F[G[B]] =
//        f.flatMap(m) {
//          ga => g.flatMap(ga)(h) // Does not work because flatMap here expects a G[_] but not a F[_]
//        }
//    }

  object Instances {
    val genMonad = new Monad[Gen] {
      override def unit[A](a: => A) = Gen.unit(a)
      override def flatMap[A, B](m: Gen[A])(f: A => Gen[B]) = m flatMap f
    }
    val parMonad = new Monad[Par] {
      override def unit[A](a: => A) = Par.unit(a)
      override def flatMap[A, B](m: Par[A])(f: A => Par[B]) = m flatMap f
    }
    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A) = Some(a)
      override def flatMap[A, B](m: Option[A])(f: A => Option[B]) = m flatMap f
    }
    val streamMonad = new Monad[Stream] {
      override def unit[A](a: => A) = Stream(a) // Not lazy over a
      override def flatMap[A, B](m: Stream[A])(f: A => Stream[B]): Stream[B] = m flatMap f
    }
    val listMonad = new Monad[List] {
      override def unit[A](a: => A) = a :: Nil
      override def flatMap[A, B](m: List[A])(f: A => List[B]) =  m flatMap f
    }
    def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](m: Either[E, A])(f: A => Either[E, B]): Either[E, B] = m flatMap f
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
      override def unit[A](a: => A) = Id(a)
      override def flatMap[A, B](m: Id[A])(f: A => Id[B]): Id[B] = f(m.value)
    }
  }
}
