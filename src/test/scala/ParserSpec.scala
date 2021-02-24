package sfpbook.ch9

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.exist.and
import org.scalatest.matchers.should._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  type Parser[A] = Int
  lazy val p: Parsers[String, Parser] = ???
  "Parsers" should "parses simple character" in {
    forAll { (c1: Char, c2: Char) => whenever(c1 != c2) {
      import p._
      run(c1)(c1.toString) shouldEqual Right(c1)
      run(c1)(c2.toString) shouldBe a [Left[_, _]]
    }}
  }
  it should "parses simple string" in {
    forAll { (s1: String, s2: String) => whenever(s1 != s2) {
      import p._
      run(s1)(s1) shouldEqual Right(s1)
      run(s1)(s2) shouldBe a[Left[_, _]]
    }}
  }
  it should "| operator parses either string" in {
    forAll { (s1: String, s2: String, s3: String) => whenever(s3 != s1 && s3 != s2) {
      import p._
      run(s1 | s2)(s1) shouldEqual Right(s1)
      run(s1 | s2)(s2) shouldEqual Right(s2)
      run(s1 | s2)(s3) shouldBe a[Left[_, _]]
    }}
  }
  it should "exactly parses multiple of" in {
    forAll { (s1: String, s2: String) =>
      import p._
      val pattern = (s1 | s2).exactly(2)
      for { i <- s1 :: s2 :: Nil; j <- s1 :: s2 :: Nil } yield {
        run(pattern)(i + j) shouldEqual Right(i + j)
      }
    }
  }
  it should "succeed always return true" in {
    forAll { (s: String) =>
      import p._
      run(succeed(true))(s) shouldEqual Right(true)
    } }
  it should "alwaysFail always return error" in {
    forAll { (s: String) =>
      import p._
      run(alwaysFail[Unit])(s) shouldBe a[Left[_, _]]
    } }
  it should "empty and nonEmpty query the string" in {
    p.run(p.empty)("") shouldEqual true
    p.run(p.empty)("a") shouldEqual false
    p.run(p.nonEmpty)("") shouldEqual false
    p.run(p.nonEmpty)("a") shouldEqual true
  }
  it should "noop returns the string unparsed" in {
    forAll { (s: String) =>
    import p._
    run(noop)(s) shouldEqual s
  } }

  object Laws {
    import p._
    def ===[A](p1: Parser[A], p2: Parser[A])(implicit g: Gen[String]) = forAll(g){ s => run(p1)(s) == run(p2)(s) }

    def mapLaw[A](p1: Parser[A])(implicit g: Gen[String]) =
      p1 === p1.map((a: A) => a)

    def flatMapLaw[A](p1: Parser[A])(implicit g: Gen[String]) =
      p1 === p1.flatMap[A](_ => p1)

    def exactly1Law[A](p1: Parser[A])(implicit g: Gen[String]) =
      p1 === p1.exactly(1)

    def exactlyDistributionLaw[A](p1: Parser[A])(implicit g: Gen[String]) =
      forAll { (a: Int, b: Int) => whenever(a >= 0 && b >= 0) {
        p1.exactly(a * b) === p1.exactly(a).exactly(b)
      } }

    def manyDistributionLaw[A](p1: Parser[A])(implicit g: Gen[String]) =
      forAll { (a: Int, b: Int, c: Int, d: Int) =>
        whenever(0 <= a && a <= b && 0 <= c && c <= d) {
          p1.many(a, b).many(c, d) === p1.many(a*c, b*d)
        } }

    def orReflectiveLaw[A](p1: Parser[A])(implicit g: Gen[String]) =
      p1 === (p1 | p1)
    def orAssociativeLaw[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(implicit g: Gen[String]) =
      (p1 | (p2 | p3)) === ((p1 | p2) | p3)

  }
}
