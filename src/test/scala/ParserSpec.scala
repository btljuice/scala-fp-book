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
    forAll { (c1: Char, c2: Char) => (c1 != c2) ==> {
      p.run(p.char(c1))(c1.toString) shouldEqual Right(c1)
      p.run(p.char(c1))(c2.toString) shouldBe a [Left[_, _]]
      p.run(p.char(c1))(c2.toString).isInstanceOf[Left[_, _]] shouldEqual true
    }}
  }
  it should ".string parses simple string" in {
    forAll { (s: String) => p.run(p.string(s))(s) shouldEqual Right(s) }
  }
  it should ".or parses either string" in {
    forAll { (s1: String, s2: String) =>
      val orP = p.or(p.string(s1), p.string(s2))
      p.run(orP)(s1) shouldEqual Right(s1)
      p.run(orP)(s2) shouldEqual Right(s2)
    }
  }
}
