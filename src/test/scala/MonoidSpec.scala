package sfpbook.ch10

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.exist.and
import org.scalatest.matchers.should._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MonoidSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "stringConcat" should "concatenate strings" in {
    forAll { ls: List[String] =>
      implicit val m = Monoid.Instances.stringConcat
      Monoid.foldMap(ls)(identity) shouldEqual ls.reduceOption(_ + _).getOrElse("")
    }
  }
  "bag" should "compute freq count" in {
    val elements = List("a", "rose", "is", "a", "rose")
    implicit val m = Monoid.mergeMapMonoid[String, Int](Monoid.Instances.intAddition)
    val actual = Monoid.foldMap(elements)(k => Map(k -> 1))
    actual shouldEqual elements.groupBy(identity).mapValues(_.size)
  }
}
