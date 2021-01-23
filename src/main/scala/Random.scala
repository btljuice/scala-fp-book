package sfpbook

import scala.annotation.tailrec


trait RNG { def next: (Int, RNG) }

case class SimpleRNG(seed: Long) extends RNG {
  override def next = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG =  SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
object RNG {
  type Rand[+A] = RNG => (A, RNG)
  implicit class RichRand[A](r: Rand[A]) {
    def map[B](f: A => B): Rand[B] = rng => { // Ch 6.5
      val (a, rng1) = r(rng)
      (f(a), rng1)
    }
    def flatMap[B](f: A => Rand[B]): Rand[B] = rng => {
      val (b, rng1) = r.map(f)(rng)
      b(rng1)
    }
    def map2[B, C](rb: => Rand[B])(f: (A, B) => C): Rand[C] = for { a <- r ; b <- rb } yield { f(a, b) }
  }
  object Rand {
    val int: Rand[Int] = _.next
    val nonNegativeInt: Rand[Int] = Rand.int.map { i => if (i == Int.MinValue) 0 else i.abs } // Ch 6.1
    val double: Rand[Double] = nonNegativeInt.map { i => i.toDouble/(Int.MaxValue.toDouble+1.0) } // Ch 6.2
    def value[A](a: => A): Rand[A] = rng => (a, rng)
    def both[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)] = a.map2(b)((_, _))
    def sequence[A](s: List[Rand[A]]): Rand[List[A]] = s match {
      case Nil => value(Nil)
      case Cons(ra, t) => ra.map2(sequence(t)) { _ :: _ }
    }
  }

  /** Ch 6.4 */
  def ints(n: Int)(r: RNG): (List[Int], RNG) = {
    @tailrec def impl(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) =
      if (n <= 0) (acc, r)
      else {
        val (i, r1) = r.next
        impl(n-1, i :: acc, r1)
      }
    impl(n, Nil, r)
  }
}
