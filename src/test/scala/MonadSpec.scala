package sfpbook.ch11

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.exist.and
import org.scalatest.matchers.should._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MonadSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "optionMonad" should "replicateM generate some" in {
    val m = Monad.Instances.optionMonad
    m.replicateM(5, Some(1)) shouldEqual Some(List.fill(5)(1))
    m.replicateM(5, None) shouldEqual None
  }
  it should "filter values" in {
    val m = Monad.Instances.optionMonad
    val input = List.range(1, 5)
    // Some(true|false) works as a normal filter
    m.filterM(input)(i => if (i % 2 == 0) Some(true) else Some(false)) shouldEqual Some(2 :: 4 :: Nil)
    // If one None is returned, it propagates as the return value
    m.filterM(input)(i => if (i == 4) None else Some(true)) shouldEqual None

  }
  "listMonad" should "replicateM generate all permutations" in {
    val m = Monad.Instances.listMonad
    val input = 1 :: 2 :: 3 :: Nil
    val expected = for { i <- 1 to 3; j <- 1 to 3 } yield i :: j :: Nil
    m.replicateM(2, input) shouldEqual expected.toList
  }
  it should "map2 generate all permutations" in {
    val m = Monad.Instances.listMonad
    val i1 = List.range(1, 4)
    val i2 = List.range(4, 7)
    m.map2(i1, i2) { (_, _) } shouldEqual ( for { i <- i1; j <- i2 } yield (i, j) )
  }
  it should "sequence generate all permutations" in {
    val m = Monad.Instances.listMonad
    val input = List(1 :: 2 :: Nil, 3 :: 4 :: Nil, 5 :: 6 :: Nil)
    m.sequence(input) shouldEqual ( for { i <- input(0); j <- input(1); k <- input(2) } yield i :: j :: k :: Nil )
  }
  it should "filter values w/ all combination" in {
    val m = Monad.Instances.listMonad
    val input = List.range(1, 4)
    val expected = List(
      1 :: 2 :: 3 :: Nil,
      1 :: 2 :: Nil,
      1 :: 3 :: Nil,
      1 :: Nil,
      2 :: 3 :: Nil,
      2 :: Nil,
      3 :: Nil,
      Nil
    )
    m.filterM(input)(_ => List(true, false)) shouldEqual expected

  }
}
