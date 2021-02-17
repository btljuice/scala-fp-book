package sfpbook.ch8

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.ch7.Par
import sfpbook.ch7.Par.Par
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

  "Ch8.14" should "List.sorted" in {
    val sortedProp = Prop.forAll(SGen.listOf(Gen.int)) { l =>
      val sl = l.sorted
      val isOrdered = sl.size <= 1 || sl.sliding(2).forall { case x :: y :: Nil => x <= y }
      val sameSize = sl.size == l.size
      val sameElements = sl.groupBy(identity) == l.groupBy(identity)
      isOrdered && sameSize && sameElements
    }
    sortedProp.execute() shouldEqual "+ OK, passed 100 tests."
  }

  lazy implicit val S: Gen[ExecutorService] = Gen.weighted(
    .75, Gen.choose(1,4).map(Executors.newFixedThreadPool),
    .25, Gen.unit(Executors.newCachedThreadPool),
  )
  "Testing" should "future" in {

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = Prop.forAll(S ** g) { case (s, a) => f(a).run(s).get }
    def checkPar[A](p: => Par[Boolean]): Prop = Prop.forAll(S) {  p.run(_).get }

    val p3 = checkPar { Par.equal(Par.unit(1).map(_ + 1), Par.unit(2)) }
    p3.execute() shouldEqual "+ OK, passed 100 tests."

    val p4 = forAllPar(Gen.int.map(Par.unit)) { p => Par.equal(p.map(identity), p) }
    p4.execute() shouldEqual "+ OK, passed 100 tests."

    p3 && p4
  }
}
