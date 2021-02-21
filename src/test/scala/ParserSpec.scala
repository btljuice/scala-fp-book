package sfpbook.ch9

import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.exist.and
import org.scalatest.matchers.should._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  type Parser[A] = Int
  val p: Parsers[String, Parser] = ???
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
  it should "listOfN parses multiple of" in {
    forAll { (s1: String, s2: String) =>
      import p._
      val pattern = listOfN(2, s1 | s2)
      for { i <- s1 :: s2 :: Nil; j <- s1 :: s2 :: Nil } yield {
        run(pattern)(i + j) shouldEqual Right(i + j)
      }
    }
  }
  // `or` Laws
  // reflective: x | x === x
  // commutative: x | y === y | x
  // associative: x | (y | z) === (x | y) | z
  //              alwaysFail | x === x
  //              identity | x === identity

  // `listOfN` Laws
  // listOfN(1, x) === x
  // listOfN(a, listOfN(b, x)) === listOfN(a * b, x)
}
