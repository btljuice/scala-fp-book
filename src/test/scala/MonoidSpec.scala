package sfpbook.ch10

import org.scalacheck.Arbitrary
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

  MonoidLaw.test("stringConcat", Monoid.Instances.stringConcat)
  MonoidLaw.test("listConcat", Monoid.Instances.listConcat[Int]) // ANSME: How to test over arbitrary types?
  MonoidLaw.test("intAddition", Monoid.Instances.intAddition)
  MonoidLaw.test("intMultiplication", Monoid.Instances.intMultiplication)
  MonoidLaw.test("booleanOr", Monoid.Instances.booleanOr)
  MonoidLaw.test("booleanAnd", Monoid.Instances.booleanAnd)
  MonoidLaw.test("optionOrElse", Monoid.Instances.optionOrElse[Int])
  MonoidLaw.testFnc[Int]("compose", Monoid.Instances.compose[Int])
  MonoidLaw.testFnc[Int]("andThen", Monoid.Instances.andThen[Int])


  object MonoidLaw {
    def test[A](label: String, m: Monoid[A])(implicit arb: Arbitrary[A]) = {
       label + " monoid instance" should "be associative" in {
         forAll { (a: A, b: A, c: A) =>
           m.op(a, m.op(b, c)) shouldEqual m.op(m.op(a, b), c)
         }
       }
       it should "zero act as identity" in {
         forAll { a: A =>
           m.op(m.zero, a) shouldEqual a
           m.op(a, m.zero) shouldEqual a
         }
       }
    }

    def testFnc[A](label: String, m: Monoid[A => A])(implicit arb: Arbitrary[A], arbf: Arbitrary[A => A]) = {
      label + " monoid instance" should "be associative" in {
        forAll { (f: A => A, g: A => A, h: A => A) =>
          val l = m.op(f, m.op(g, h))
          val r = m.op(m.op(f, g), h)
          forAll { a: A => l(a) shouldEqual r(a) }
        }
      }
      it should "zero act as identity" in {
        forAll { (f: A => A) =>
          val l = m.op(f, m.zero)
          val r = m.op(m.zero, f)
          forAll { a: A =>l(a) shouldEqual f(a); r(a) shouldEqual f(a) }
        }
      }
    }
  }
}
