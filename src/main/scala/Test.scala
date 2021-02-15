package sfpbook.ch8
import sfpbook.ch6.Random
import sfpbook.ch6.Random.RNG
import sfpbook.ch6.State


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

object Test {
  type Gen[A] = Random.Rand[A]
  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Random.choose(start, stopExclusive)
    def unit[A](a: => A): Gen[A] = State.value[RNG, A](a)
    def boolean: Gen[Boolean] = Random.range(2).map { _ == 1 }
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = State.sequence(List.fill(n)(g))
  }
  sealed trait Prop { self =>
    import Prop._
    def check: Either[FailedCase, SuccessCount]
    final def &&(p: Prop): Prop = new Prop { // Ex 8.3
      def check = self.check match {
        case l@Left(_) => l
        case Right(s0) => p.check match {
          case Left((failedCase, s1)) => Left((failedCase, s0 + s1))
          case Right(s1) => Right(s0 + s1)
        }
      }
    }
  }
  object Prop {
    type FailedCase = (String, SuccessCount)
    type SuccessCount = Int
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

}