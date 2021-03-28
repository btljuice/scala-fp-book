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
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  // Implement either sequence or traverse
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit ag : Applicative[G]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  // Because map can be implemented in terms of traverse, Traverse is also a functor.
  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    implicit val ia = Applicative.Instances.idApplicative
    traverse(fa){ a => ia.unit(f(a)) }.value
  }

  // Foldable implementation
  override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(a => f(a, _))(Monoid.Instances.compose[B])(z)
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => f(_, a))(Monoid.Instances.andThen[B])(z)
  override def foldMap[A, B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B =
    traverse[({type f[x] = Applicative.Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(m))

  // State traversal helper
  final def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type s[x] = State[S, x]})#s, A, B](fa)(f)(Monad.Instances.stateMonad[S])

  final def mapAccum[S,A,B](s: S, fa: F[A])(f: (S, A) => (S, B)): (S, F[B]) = traverseS(fa) { a => for {
    s1 <- State.get[S]
    (s2, b) = f(s1, a)
    _ <- State.set(s2)
  } yield b }(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(0, fa)((i, a) => (i + 1, (a, i)))._2

  private[this] def toReverseList[A](fa: F[A]): List[A] = mapAccum(List.empty[A], fa)((l, a) => (a :: l, ()))._1

  // override because Foldable also has its toList implementation
  // Any traversable functor can be converted to a list
  override def toList[A](fa: F[A]): List[A] = toReverseList(fa).reverse

  def reverse[A](fa: F[A]): F[A] = {
    val rl = toReverseList(fa)
    mapAccum(rl, fa)((l, _) => (l.tail, l.head))._2
  }

  def size[A](fa: F[A]): Int = mapAccum(0, fa) { case (i, _) => (i+1, ()) }._1

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = mapAccum(toList(fb), fa) {
    case (Nil, a) => sys.error("zip: Incompatible shapes.")
    case (b :: bs, a) => (bs, (a, b))
  }._2

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = mapAccum(toList(fb), fa) {
    case (Nil, a) => (Nil, (a, None))
    case (b :: bs, a) => (bs, (a, Some(b)))
  }._2

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = mapAccum(toList(fa), fb) {
    case (Nil, b) => (Nil, (None, b))
    case (a :: as, b) => (as, (Some(a), b))
  }._2

  def fuse[G[_], H[_], A, B](fa: F[A])(g: A => G[B], h: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    implicit val GH = Applicative.product(G, H)
    type t[B] = (G[B], H[B])
    traverse[t, A, B](fa){ a => g(a) -> h(a) }
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    type FG[A] = F[G[A]]
    new Traverse[FG] {
      override def traverse[H[_], A, B](fga: FG[A])(f: A => H[B])(implicit ah: Applicative[H]): H[FG[B]] =
        self.traverse(fga)( G.traverse(_)(f) )
    }
  }
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
