package sfpbook.ch12

import sfpbook.ch3.TreeList

trait Traverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] // sequence(af.map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
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
