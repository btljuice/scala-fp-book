package sfpbook.ch10

/** Requirements:
 * 1. op must be associative === op(a, op(b, c)) == op(op(a, b), c)
 * 2. op(zero, x) == x && op(x, zero) == x */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)) = (ma.op(x._1, y._1), mb.op(x._2, y._2))
    def zero = (ma.zero, mb.zero)
  }

  def mergeMapMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def op(m1: Map[K, V], m2: Map[K, V]) =
      (m1.keySet ++ m2.keySet).foldLeft(Map.newBuilder[K, V]) { (acc, k) =>
        val v = m.op(m1.getOrElse(k, m.zero), m2.getOrElse(k, m.zero))
        acc += k -> v
      }.result

    def zero = Map.empty[K, V]
  }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B) = a => m.op( f1(a), f2(a))
    def zero = _ => m.zero
  }


  def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B =
    as.foldLeft(m.zero){ (b, a) => m.op(b, f(a)) }

  def foldMapV[A, B](as: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = as.size match {
    case 0 => m.zero
    case 1 => f(as.head)
    case n =>
      val (l, r) = as.splitAt(n/2)
      m.op( foldMapV(l)(f), foldMapV(r)(f) )
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => f(a, _))(Instances.compose[B])(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => f(_, a))(Instances.andThen[B])(z)

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
    def compose[A] = new Monoid[A => A] {
      def op(f1: A => A, f2: A => A) = a => f1(f2(a))
      val zero = identity
    }
    def andThen[A] = new Monoid[A => A] {
      def op(f1: A => A, f2: A => A) = a => f2(f1(a))
      val zero = identity
    }
  }
}
