package sfpbook.ch8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.ch8.Test._

class Ch8Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "Ch8.1" should "simple test example" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      lazy val max = ns.max
      !ns.exists(_ > max)
    }
    maxProp.execute() shouldEqual "+ OK, passed 100 tests."
  }
}
