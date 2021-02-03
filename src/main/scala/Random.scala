package sfpbook.ch6

object Random {
  trait RNG { def next: (RNG, Int) }

  case class SimpleRNG(seed: Long) extends RNG {
    override def next = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG =  SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (nextRNG, n)
    }
  }

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State[RNG, Int](_.next)
  val nonNegativeInt: Rand[Int] = int.map { i => if (i == Int.MinValue) 0 else i.abs } // Ch 6.1
    val double: Rand[Double] = nonNegativeInt.map { i => i.toDouble/(Int.MaxValue.toDouble+1.0) } // Ch 6.2
    def range(n: Int): Rand[Int] = nonNegativeInt.flatMap { i =>
      val mod = i % n
      if (i-mod + n-1 >= 0) State.value(mod) else range(n)
    }

  def ints(n: Int): Rand[List[Int]] = State.sequence(List.fill(n)(int))
}
