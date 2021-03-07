package sfpbook.ch11

/** Requirements:
 * 1. map(x)(identity) == x */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  final def distribute[A, B](f: F[(A, B)]): (F[A], F[B]) = ( map(f)(_._1) , map(f)(_._2) )
  final def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
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
