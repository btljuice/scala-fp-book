package sfpbook.ch9

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.io.Source.fromResource

class ParserSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  def nonNullChar(s: String*) = s.forall(_.forall(_ != '\0'))
  lazy val p: Parsers[SimpleParser.ParseError, SimpleParser] = SimpleParsers

  "char()" should "succeed when matches 1st char" in {
    forAll { (c1: Char, s: String) => whenever(!s.startsWith(c1.toString)) {
      import p._
      p.run(c1)(c1 + s) shouldEqual Right(c1)
      p.run(c1)(s) shouldBe a [Left[_, _]]
    }}
  }
  "string(\"\")" should "always succeed" in {
    forAll { (s1: String) => {
      import p._
      p.run("")(s1) shouldEqual Right("")
    } }
  }
  "string(.+)" should "succeed when input startsWith" in {
    forAll { (s1: String, s2: String) => whenever(s1.nonEmpty && !s2.startsWith(s1.head.toString)) {
      import p._
      p.run(s1)(s2) shouldBe a [Left[_, _]]
      p.run(s1)(s1 + s2) shouldEqual Right(s1)
    } }
  }
  "spaces" should "succeed at parsing all strings" in {
    forAll { (i: Int) => whenever (0 <= i && i <= 100) {
      import p._
      val input = List.fill(i)(' ').mkString
      p.run(spaces)(input) shouldBe a [Right[_,_]]
    }}

  }
  "| operator" should "parse either string" in {
    forAll { (s1: String, s2: String, s3: String) => whenever(s1 != s3 && s2 != s3 && s1.nonEmpty && s2.nonEmpty) {
      import p._
      p.run(attempt(s1) | s2)(s1) shouldEqual Right(s1)
      p.run(attempt(s1) | s2)(s2) shouldEqual Right(s2)
      p.run(attempt(s1) | s2)(s3) shouldBe a[Left[_, _]]
    }}
  }
  "** operator" should "concatenate parsing" in {
    forAll { (s1: String, s2: String) => whenever(s1.nonEmpty && s2.nonEmpty) {
      import p._
      p.run(string(s1) ** s2)(s1 + s2) shouldEqual Right((s1, s2))
      p.run(string(s1) ** s2)(s1 + " " + s2) shouldBe a [Left[_, _]]
    } }
  }
  "exactly" should "matches exactly number specified" in {
    forAll { (s1: String, n: Int) => whenever(0 <= n && n <= 10) {
      import p._
      val pattern = string(s1).exactly(n)
      val patternOneTooMany = string(s1).exactly(n + 1) //
      p.run(pattern)(s1 * n) shouldEqual Right(List.fill(n)(s1))
      p.run(patternOneTooMany)(s1 * n) shouldBe a [Left[_, _]]
    } }
  }
  "succeed" should "always return value" in {
    forAll { (a: Int, s: String) =>
      p.run(p.succeed(a))(s) shouldEqual Right(a)
    }
  }
  "alwaysFail" should "always return error" in {
    forAll { (s: String) =>
      p.run(p.alwaysFail[Unit])(s) shouldBe a[Left[_, _]]
    }
  }
  "noop" should "return the string unparsed" in {
    forAll { (s: String) =>
      p.run(p.noop)(s) shouldEqual Right(s)
    }
  }

  "Example dateAndTemperatureParser" should "parse dates ans temperature" in {
    val dateAndTemperatureParser: SimpleParser[(Long, Long, Long, Long)] = {
      import p._
      def longRange(min: Long, max: Long): SimpleParser[Long] = p.long.flatMap { l =>
        if (min <= l && l <= max) p.succeed(l)
        else label(s"Long=$l is outside of [$min, $max]")(alwaysFail)
      }

      val date = (longRange(1, 31) ** '/' ** longRange(1, 12) ** '/' ** longRange(2000, 3000))
        .map { case ((((d, _), m), _), y) => (d, m, y) }

      separated3(spaces)(date, ',', p.long).map { case ((d, m, y), _, t) => (d, m, y, t) }
    }
    val file = fromResource("DateAndTemperature.txt").getLines.toList
    val result1 = file
      .map(s => dateAndTemperatureParser.run(Location(s)))
    val result2 = result1
      .collect { case SimpleParser.ParseSuccess((d, m, y, t), _) => (d, m, y, t) }
    val expected = List(
      (1, 1, 2010, 25),
      (2, 1, 2010, 28),
      (3, 1, 2010, 42),
      (4, 1, 2010, 53),
      (5, 1, 2010, 45),
      (6, 1, 2010, 35),
    )

    result2 shouldEqual expected
  }


  object LawsParser {
    import p._
    def ===[A](p1: ParserType[A], p2: ParserType[A])(implicit g: Gen[String]) =
      forAll(g){ s => p.run(p1)(s) == p.run(p2)(s) }

    def mapLaw[A](p1: ParserType[A])(implicit g: Gen[String]) =
      p1 === p1.map((a: A) => a)

    def flatMapLaw[A](p1: ParserType[A])(implicit g: Gen[String]) =
      p1 === p1.flatMap[A](_ => p1)

    def exactly1Law[A](p1: ParserType[A])(implicit g: Gen[String]) =
      p1 === p1.exactly(1)

    def exactlyDistributionLaw[A](p1: ParserType[A])(implicit g: Gen[String]) =
      forAll { (a: Int, b: Int) => whenever(a >= 0 && b >= 0) {
        p1.exactly(a * b) === p1.exactly(a).exactly(b)
      } }

    def manyDistributionLaw[A](p1: ParserType[A])(implicit g: Gen[String]) =
      forAll { (a: Int, b: Int, c: Int, d: Int) =>
        whenever(0 <= a && a <= b && 0 <= c && c <= d) {
          p1.many(a, b).many(c, d) === p1.many(a*c, b*d)
        } }

    def orReflectiveLaw[A](p1: ParserType[A])(implicit g: Gen[String]) =
      p1 === (p1 | p1)
    def orAssociativeLaw[A](p1: ParserType[A], p2: ParserType[A], p3: ParserType[A])(implicit g: Gen[String]) =
      (p1 | (p2 | p3)) === ((p1 | p2) | p3)


    def labelLaw[A](pa: ParserType[A])(implicit g: Gen[String]) =
      forAll(g, Gen.asciiStr) { (input, msg) =>
        import p._
        p.run(label(msg)(pa))(input) match {
          case Left(e) => errorMessage(e) shouldEqual msg
          case _ => true
        }
      }
  }
}
