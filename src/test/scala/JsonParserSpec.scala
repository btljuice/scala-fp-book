package sfpbook.ch9

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.io.Source.fromResource
import sfpbook.ch9.SimpleParser.ParseSuccess

class JsonParserSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  lazy val jsonParser = Json.parser(SimpleParsers)
  "json" should "parse null" in {
    jsonParser.run(Location("null")) shouldEqual ParseSuccess(Json.JNull, 4)
  }
}
