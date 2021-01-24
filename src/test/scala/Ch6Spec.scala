package sfpbook

import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.RNG._
import sfpbook.RNG.RichRand

class Ch6Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "Ch6.1" should "generate non-strict positive Int" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      Rand.nonNegativeInt(gen)._1 should be >= 0
    }
  }
  "Ch6.2" should "generate double between 0.0 and 1.0" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      Rand.double(gen)._1 should (be >= 0.0 and be < 1.0)
    }
  }
  "Ch6.4" should "generate list of integers" in {
    forAll { (n: Int, s: Int) =>
      (n >= 0) ==> {
        val gen = SimpleRNG(s)
        val (l, _) = RNG.ints(n)(gen)
        l.length == n
      }
    }
  }
  /** @todo what general properties should be tested for map? */
  "Ch6.5" should "map" in {
    forAll { (s: Int, f: Int => Int, g: Int => Int) => // ANSME: How those function are generated?
      val gen = SimpleRNG(s)
      f(Rand.int(gen)._1) shouldEqual Rand.int.map(f)(gen)._1
      Rand.int.map(f).map(g)(gen) shouldEqual Rand.int.map(x => g(f(x)))(gen)
    }
  }
  "Ch6.6" should "flatMap" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      Rand.int.flatMap(i => Rand.value(i))(gen) shouldEqual Rand.int(gen)
    }
  }
  "Ch6.6" should "map2" in {
    forAll { (s: Int) =>
      val gen = SimpleRNG(s)
      val aPlusB = {
        val (a, gen1) = Rand.int(gen)
        val (b, gen2) = Rand.int(gen1)
        (a + b, gen2)
      }
      Rand.int.map2(Rand.int)(_ + _)(gen) shouldEqual aPlusB
    }
  }
  "Ch6.9" should "range" in {
    forAll { (s: Int, n: Int) =>
      (n >= 0) ==> {
        val gen = SimpleRNG(s)
        val r = Rand.range(n)(gen)._1
        0 <= r && r < n
      }
    }
  }
}
