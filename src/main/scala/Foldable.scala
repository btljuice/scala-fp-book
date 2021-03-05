package sfpbook.ch10

import scala.annotation.tailrec

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B

  def concatenate[A](as: F[A])(implicit m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object Foldable {
  object Instances {
    @tailrec private def foldLeftIte[A, B](as: Iterator[A])(z: B)(f: (B, A) => B): B =
      if (as.hasNext) {
        val a = as.next()
        foldLeftIte(as)(f(z, a))(f)
      } else z

    val listFoldable = new Foldable[List] {
      def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = foldLeftIte(as.reverseIterator)(z)((b, a) => f(a, b))
      def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = foldLeftIte(as.iterator)(z)(f)
      def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]) = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
    }

    val indexedSeqFoldable = new Foldable[IndexedSeq] {
      def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = foldLeftIte(as.reverseIterator)(z)((b, a) => f(a, b))
      def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = foldLeftIte(as.iterator)(z)(f)
      def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = as.size match {
        case 0 => m.zero
        case 1 => f(as.head)
        case n =>
          val (l, r) = as.splitAt(n/2)
          m.op( foldMap(l)(f), foldMap(r)(f) )
      }
    }

    val streamFoldable = new Foldable[Stream] {
      def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as match {
        case Stream.Empty => z
        case s: Stream.Cons[A] => f(s.head, foldRight(s.tail)(z)(f))
      }
      @tailrec def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as match {
        case Stream.Empty => z
        case a #:: t => foldLeft(t)(f(z, a))(f)
      }
      def foldMap[A, B](as: Stream[A])(f: A => B)(implicit m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
    }
  }
}
