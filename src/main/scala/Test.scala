package sfpbook.ch8

import sfpbook.ch6.Random
import sfpbook.ch6.Random.RNG
import sfpbook.ch6.State
import sfpbook.ch5.Stream
import sfpbook.ch6.Random.SimpleRNG


// Ex 8.1
// Property of
// sum(l: List[Int]): Int
// 1. Associativity a + (b + c) == (a + b) + c
//    sum(sum(a ::: b) :: c) == sum(a :: sum(b ::: c) :: Nil)
// 2. commutativity a + b == b + a
//    sum(a ::: b) == sum(b ::: a)
// 3. Order is not important
//    sum(a) == sum(shuffle(a))
// 4. Decomposability
//    sum(a ::: b ::: c) == sum(a) + sum(b) + sum(c)
// 5. Identity
//    sum(Nil) == 0

// Ex 8.2
// Property of
// max(l: List[Int]): Int
// 1. associativity
//    max(a, max(b, c)) == max(max(a, b), c)
// 2. commutativity
//    max(a, b) == max(b, a)
// 3. Order is not important
//    max(a) == max(shuffle(a))
// 4. Adding an new element:
//    max(a, b) <= max(a, max(b, c))
// 5. Max property
//    max(a, b) = if (a < b) b else a
// 6. Identity value
//    max(a, MIN_VALUE) = a

object Test {
  type Gen[A] = Random.Rand[A]
  implicit class RichGen[A](g: Gen[A]) {
    def listOfN(gn: Gen[Int]): Gen[List[A]] = gn.flatMap(Gen.listOfN(_, g))
    def unsized: SGen[A] = SGen(_ => g)
  }
  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Random.choose(start, stopExclusive)
    def unit[A](a: => A): Gen[A] = State.value[RNG, A](a)
    def boolean: Gen[Boolean] = Random.range(2).map { _ == 1 }
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = State.sequence(List.fill(n)(g))
    def union[A, B](g0: Gen[A], g1: Gen[A]): Gen[A] = boolean.flatMap { if (_) g0 else g1 }
    def weighted[A](w0: Double, g0: Gen[A], w1: Double, g1: Gen[A]): Gen[A] = Random.double.flatMap { d => val p0 = w0 / (w0 + w1); if (p0 < d) g0 else g1 }
  }

  case class SGen[A](forSize: Int => Gen[A]) {
    def apply(i: Int): Gen[A] = forSize(i)
  }
  object SGen {
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i + 1, g))
  }

  import Prop._
  case class Prop(run: (MaxSize, TestCases, RNG) => Prop.Result) {
    def &&(p: Prop): Prop = Prop( (ms, n, rng) => { val r0 = run(ms, n, rng); if ( r0.isFalsified) r0 else p.run(ms, n, rng) })
    def ||(p: Prop): Prop = Prop( (ms, n, rng) => { val r0 = run(ms, n, rng); if (!r0.isFalsified) r0 else p.run(ms, n, rng) })
    def execute(maxSize: Int = 100, testCases: Int= 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): String =
      run(maxSize, testCases, rng) match {
        case Passed => s"+ OK, passed $testCases tests."
        case Falsified(msg, n) => s"! Falsified after $n passed tests:\n $msg"
      }
  }
  object Prop {
    type MaxSize = Int
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int
    sealed trait Result { def isFalsified: Boolean }
    case object Passed extends Result { def isFalsified = false }
    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result { def isFalsified = true }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
      val casesPerSize = (n - 1 + max) / max
      val props = Stream.from(0).take(Math.min(n, max) + 1).map(i => forAll(g(i))(f))
      val prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map { case (a, i) =>
        try { if (f(a)) Passed else Falsified(a.toString, i) }
        catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
    }
    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)( rng => Some(g.run(rng).swap))
    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  }

}
