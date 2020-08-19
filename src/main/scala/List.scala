package sfpbook

object Ch3 {
  sealed trait List[+T] {
    def tail: List[T]
    def isEmpty: Boolean
    def nonEmpty: Boolean = !isEmpty
    def length: Int

    def forall[A >: T](f: A => Boolean): Boolean
    def exists[A >: T](f: A => Boolean): Boolean

    def drop(n: Int): List[T]
    def dropWhile[A >: T](f: A => Boolean): List[T]
    def take(n: Int): List[T]
    def takeWhile[A >: T](f: A => Boolean): List[T]

    def ::[A >: T](prefix: A): List[A]
    def :::[A >: T](l: List[A]): List[A]

    def foldRight[A](z: A)(f: (A, T) => A): A
    def foldLeft[A](z: A)(f: (A, T) => A): A

    def reverse: List[T]
    def map[A](f: T => A): List[A]
    def filter[A >: T](f: A => Boolean): List[T]
    def flatMap[A](f: T => List[A]): List[A]
    def zipWith[A, B](l: List[A])(f: (T, A) => B): List[B]

  }

  final case object Nil extends List[Nothing] {
    override def tail = this
    override def isEmpty = true
    override def length: Int = 0

    override def forall[A](f: A => Boolean) = true
    override def exists[A](f: A => Boolean) = false

    override def drop(n: Int) = this
    override def dropWhile[A](f: A => Boolean) = this
    override def take(n: Int) = this
    override def takeWhile[A](f: A => Boolean) = this

    override def ::[A](prefix: A) = Cons(prefix, this)
    override def :::[A](l: List[A]) = l

    override def foldRight[A](z: A)(f: (A, Nothing) => A) = z
    override def foldLeft[A](z: A)(f: (A, Nothing) => A) = z

    override def reverse = this
    override def map[A](f: Nothing => A) = this
    override def filter[A](f: A => Boolean) = this
    override def flatMap[A](f: Nothing => List[A]) = this
    override def zipWith[A, B](l: List[A])(f: (Nothing, A) => B) = this


  }

  final case class Cons[+T](head: T, tail: List[T]) extends List[T] {
    override def isEmpty = false
    override def length: Int = foldLeft(0) { (c, _) => c + 1 }

    override def forall[A >: T](f: A => Boolean) = if (f(head)) tail.forall(f) else false
    override def exists[A >: T](f: A => Boolean) = if (f(head)) true else tail.exists(f)

    override def drop(n: Int) = if (n <= 0) this else tail.drop(n-1)
    override def dropWhile[A >: T](f: A => Boolean) = if (f(head)) tail.dropWhile(f) else this
    override def take(n: Int) = if (n <= 0) Nil else Cons(head, tail.take(n-1))
    override def takeWhile[A >: T](f: A => Boolean) = if (f(head)) Cons(head, tail.takeWhile(f)) else Nil

    override def ::[A >: T](prefix: A) = Cons(prefix, this)
//    override def :::[A >: T](l: List[A]) = l match {
//      case Nil => this
//      case Cons(head, tail) => Cons(head, tail ::: this)
//    }
    override def :::[A >: T](l: List[A]) = l.foldRight[List[A]](this) { (l, h) => Cons(h, l) }

//    override def foldRight[A](z: A)(f: (A, T) => A) = f(tail.foldRight(z)(f), head)
    override def foldRight[A](z: A)(f: (A, T) => A) = reverse.foldLeft(z)(f)
    override def foldLeft[A](z: A)(f: (A, T) => A) = tail.foldLeft(f(z, head))(f)

    override def reverse = foldLeft[List[T]](Nil){ (l, h) => Cons(h, l) }
    override def map[A](f: T => A) = foldRight[List[A]](Nil){ (l, t) => Cons(f(t), l) }
//    override def filter(f: T => Boolean) = foldRight[List[T]](Nil){ (l, t) => if (f(t)) Cons(t, l) else l }
    override def filter[A >: T](f: A => Boolean) = flatMap[T] { t => if (f(t)) Cons(t, Nil) else Nil }
    override def flatMap[A](f: T => List[A]) = foldRight[List[A]](Nil){ (l, t) => f(t) ::: l }

    override def zipWith[A, B](l: List[A])(f: (T, A) => B) = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(head, h), tail.zipWith(t)(f))
    }
  }

  object List {
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Ch3.Cons(as.head, apply(as.tail: _*))
  }
}
