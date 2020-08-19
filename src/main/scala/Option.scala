package sfpbook

sealed trait Option[+A] {
  def get: A
  def isEmpty: Boolean
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}
final object None extends Option[Nothing] {
  override def get = throw NoSuchFieldException
  def isEmpty = true
  override def map[B](f: Nothing => B) = None
  override def flatMap[B](f: Nothing => Option[B]) = None
  override def getOrElse[B >: Nothing](default: => B) = default
  override def orElse[B >: Nothing](default: => Option[B]) = default
  override def filter(f: Nothing => Boolean) = None
}
final case class Some[+A](get: A) extends Option[A] {
  def isEmpty = false
  override def map[B](f: A => B) = Some(f(get))
  override def flatMap[B](f: A => Option[B]) = f(get)
  override def getOrElse[B >: A](default: => B) = get
  override def orElse[B >: A](default: => Option[B]) = this
  override def filter(f: A => Boolean) = if (f(get)) this else None
}
object Option {
  def map2[A, B, C](ma: Option[A], mb: Option[B])(f: (A, B) => C) = for {a <- ma; b <- mb} yield f(a, b)
  def sequence[A](l: List[Option[A]]): Option[List[A]] = traverse(l)(identity)
  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] = l match {
    case Nil => Some(Nil)
    case head :: tail =>
      for {
        bHead <- f(head)
        bTail <- traverse(tail)(f)
      } yield bHead :: bTail
  }
}
