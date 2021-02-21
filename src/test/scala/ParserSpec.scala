package sfpbook.ch9

import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  type Parser[A] = Int
  val p: Parsers[String, Parser] = ???
  "Parsers" should ".char parses simple character" in {
    forAll { (c1: Char, c2: Char) => whenever(c1 != c2) {
      import p._
      run(c1)(c1.toString) shouldEqual Right(c1)
      run(c1)(c2.toString) shouldBe a [Left[_, _]]
    }}
  }
  it should ".string parses simple string" in {
    forAll { (s1: String, s2: String) => whenever(s1 != s2) {
      import p._
      run(s1)(s1) shouldEqual Right(s1)
      run(s1)(s2) shouldBe a[Left[_, _]]
    }}
  }
  it should ".or parses either string" in {
    forAll { (s1: String, s2: String, s3: String) => whenever(s3 != s1 && s3 != s2) {
      import p._
      run(s1 | s2)(s1) shouldEqual Right(s1)
      run(s1 | s2)(s2) shouldEqual Right(s2)
      run(s1 | s2)(s3) shouldBe a[Left[_, _]]
    }}
  }
}
