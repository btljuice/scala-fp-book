package sfpbook.ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  object Instances {
    val stringConcat = new Monoid[String] {
      def op(a1: String, a2: String) = a1 + a2
      def zero = ""
    }
    def listConcat[A] = new Monoid[List[A]] {
      def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
      val zero = Nil
    }
    val intAddition = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2
      val zero = 0
    }
    val intMultiplication = new Monoid[Int] {
      def op(a1: Int, a2: Int) = a1 * a2
      val zero = 1
    }
    val booleanOr = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean) = a1 || a2
      val zero = false
    }
    val booleanAnd = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean) = a1 && a2
      val zero = true
    }
    def optionOrElse[A] = new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
      val zero = None
    }
    def composeMonoid[A] = new Monoid[A => A] {
      def op(f1: A => A, f2: A => A) = a => f1(f2(a))
      val zero = identity
    }
    def andThenMonoid[A] = new Monoid[A => A] {
      def op(f1: A => A, f2: A => A) = a => f2(f1(a))
      val zero = identity
    }
  }

  def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B =
    as.foldLeft(m.zero){ (b, a) => m.op(b, f(a)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => f(a, _))(Instances.composeMonoid[B])(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => f(_, a))(Instances.andThenMonoid[B])(z)

}
