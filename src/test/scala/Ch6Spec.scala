package sfpbook.ch6

import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Random._

class Ch6Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "Ch6.1" should "generate non-strict positive Int" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      nonNegativeInt(gen)._2 should be >= 0
    }
  }
  "Ch6.2" should "generate double between 0.0 and 1.0" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      double(gen)._2 should (be >= 0.0 and be < 1.0)
    }
  }
  "Ch6.4" should "generate list of integers" in {
    forAll { (n: Int, s: Int) =>
      (n >= 0) ==> {
        val gen = SimpleRNG(s)
        val (_, l) = ints(n)(gen)
        l.length == n
      }
    }
  }
  /** @todo what general properties should be tested for map? */
  "Ch6.5" should "map" in {
    forAll { (s: Int, f: Int => Int, g: Int => Int) => // ANSME: How those function are generated?
      val gen = SimpleRNG(s)
      f(int(gen)._2) shouldEqual int.map(f)(gen)._2
      int.map(f).map(g)(gen) shouldEqual int.map(x => g(f(x)))(gen)
    }
  }
  "Ch6.6" should "flatMap" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      int.flatMap(i => State.value(i))(gen) shouldEqual int(gen)
    }
  }
  "Ch6.6" should "map2" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      val aPlusB = {
        val (gen1, a) = int(gen)
        val (gen2, b) = int(gen1)
        (gen2, a + b)
      }
      int.map2(int)(_ + _)(gen) shouldEqual aPlusB
    }
  }
  "Ch6.9" should "range" in {
    forAll { (s: Int, n: Int) =>
      (n >= 0) ==> {
        val gen = SimpleRNG(s)
        val r = range(n)(gen)._2
        0 <= r && r < n
      }
    }
  }

  "Ch6.11" should "coin exercise" in {
    // Insert coin; turn knob
    // state lock vs unlock
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    // Rules
    // 1. Insert a coin into lock machine if candy left -> Unlock
    // 2. Turn knob on unlock machine -> dispenses a candy; Lock Machine
    // 3. Turn knob on lock machine -> Nothing
    // 4. insert coin into unlock machine -> Nothing
    // 5. Machine w/o candy ignores all input
    case class Machine(locked: Boolean, candies: Int, coins: Int)


    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      def update(i: Input): Machine => Machine = { m: Machine => (i, m) match {
        case (Coin, Machine(true, a, o)) if a > 0 => Machine(false, a, o)
        case (Turn, Machine(false, a, o)) => Machine(true, a - 1, o + 1)
        case (_, m) => m
      } }
      State.sequence(
        inputs.map(update _ andThen State.modify)
      ).flatMap(_ => State.get[Machine])
        .map(m => (m.candies, m.coins))
    }

    val startMachine = Machine(true, 5, 10)
    val actions = List(Turn, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    simulateMachine(actions)(startMachine) shouldEqual (Machine(true, 1, 14), (1, 14))
  }
}
