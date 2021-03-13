package sfpbook.ch11

/** Requirements:
 * 1. map(x)(identity) == x */
trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  final def distribute[A, B](f: F[(A, B)]): (F[A], F[B]) = ( map(f)(_._1) , map(f)(_._2) )
  final def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e.fold(map(_)(Left(_)), map(_)(Right(_)))

  implicit class FunctorOps[A](fa: F[A]) {
    final def map[B](f: A => B): F[B] = self.map(fa)(f)
  }
}

object Functor {
  object Instances {
    val listFunctor = new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B) = fa map f
    }

    val optionFunctor = new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
    }
  }
}
