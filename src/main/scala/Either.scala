package sfpbook

sealed trait Either[+L, +R] {
  def map[A](f: R => A): Either[L, A]
  def flatMap[LL >: L, A](f: R => Either[LL, A]): Either[LL, A]
  def getOrElse[A >: R](default: => A): A
  def orElse[LL >: L, A >: R](default: => Either[LL, A]): Either[LL, A]
  def map2[LL >: L, A, B](a: Either[LL, A])(f: (R, A) => B): Either[LL, B]
}
case class Left[+L](left: L) extends Either[L, Nothing] {
  override def map[A](f: Nothing => A) = this
  override def flatMap[LL >: L, A](f: Nothing => Either[LL, A]) = this
  override def getOrElse[A](default: => A): A = default
  override def orElse[LL >: L, A](default: => Either[LL, A]) = default
  override def map2[LL >: L, A, B](a: Either[LL, A])(f: (Nothing, A) => B) = this

}

case class Right[+R](right: R) extends Either[Nothing, R] {
  override def map[A](f: R => A): Either[Nothing, A] = Right(f(right))
  override def flatMap[Nothing, A](f: R => Either[Nothing, A]) = f(right)
  override def getOrElse[A >: R](default: => A): A = right
  override def orElse[Nothing, A >: R](default: => Either[Nothing, A]) = this
  override def map2[LL >: Nothing, A, B](a: Either[LL, A])(f: (R, A) => B) = a.map { r2 => f(right, r2) }
}

object Either {
  def sequence[L, R](es: List[Either[L, R]]): Either[L, List[R]] = traverse(es)(Right(_))
  def traverse[L, R, A](es: List[Either[L, R]])(f: R => Either[L, A]): Either[L, List[A]] = es match {
    case Nil => Right(Nil)
    case Cons(head, tail) => for { h <- head; a <- f(h); t <- traverse(tail)(f) } yield { Cons(a, t) }
  }
}
