package sfpbook.ch12

import sfpbook.ch10.Foldable
import sfpbook.ch10.Monoid
import sfpbook.ch11.Functor
import sfpbook.ch11.Monad
import sfpbook.ch12.Applicative.monoidApplicative
import sfpbook.ch3.TreeList
import sfpbook.ch6.State

/**
 * - Traverse is a "generalization" of Functor, because map can be implemented in terms of traverse
 * - Traverse is a "generalization" of Foldable, because a Monoid can be cast to an applicative + foldMap can be
 *   implemented in terms of foldMap
 */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  // Implement either sequence or traverse
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit ag : Applicative[G]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  final def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type s[x] = State[S, x]})#s, A, B](fa)(f)(Monad.Instances.stateMonad[S])

  // Because map can be implemented in terms of traverse, Traverse is also a functor.
  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    implicit val ia = Applicative.Instances.idApplicative
    traverse(fa){ a => ia.unit(f(a)) }.value
  }

  override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(a => f(a, _))(Monoid.Instances.compose[B])(z)
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => f(_, a))(Monoid.Instances.andThen[B])(z)
  override def foldMap[A, B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B =
    traverse[({type f[x] = Applicative.Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(m))
}

object Traverse {

  object Instances {
    val optionTraverse = new Traverse[Option] {
      override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
        val ao = implicitly[Applicative[G]]
        fa.fold(ao.unit(Option.empty[B])) { a => ao.map(f(a))(Some(_)) }
      }
    }
    val listTraverse = new Traverse[List] {
      override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
        val ao = implicitly[Applicative[G]]
        ao.traverse(fa)(f)
      }
    }
    val treeListTraverse = new Traverse[TreeList] {
      override def traverse[G[_] : Applicative, A, B](fa: TreeList[A])(f: A => G[B]): G[TreeList[B]] = {
        val ag = implicitly[Applicative[G]]
        val tails: G[List[TreeList[B]]] = ag.traverse(fa.tail)(traverse(_)(f))
        ag.map2(f(fa.head), tails)(TreeList(_, _))
      }
    }
  }
}
