package sfpbook.ch12

import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.Date
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.util.Try

class ApplicativeSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "streamZipApplicative" should "sequence makes all zip" in {
    val sa = Stream(1, 2, 3, 4, 5)
    val sb = Stream(-1, -2, -3, -4, -5, -6)
    val sc = Stream.from(10, 10)
    val sd = Stream.continually(0)

    val expected = Stream(
      1 :: -1 :: 10 :: 0 :: Nil,
      2 :: -2 :: 20 :: 0 :: Nil,
      3 :: -3 :: 30 :: 0 :: Nil,
      4 :: -4 :: 40 :: 0 :: Nil,
      5 :: -5 :: 50 :: 0 :: Nil,
    )

    import Applicative.Instances.streamZipApplicative._
    sequence(sa :: sb :: sc :: sd :: Nil) shouldEqual expected
  }

  "validationApplicative" should "accumulate webform errors" in {
    import Validation._
    val a = Applicative.Instances.validationApplicative[String]
    import a._
    case class WebForm(name: String, birthdate: Date, phoneNumber: String)
    def validWebForm(n: String, bd: String, pn: String) = {
      def validName = if (n.nonEmpty) Success(n) else Failure("Name cannot be empty")
      def validBirthDate = try { Success(new SimpleDateFormat("yyyy-MM-dd").parse(bd)) }
                           catch { case _: ParseException => Failure("Birthdate must be of the form yyyy-MM-dd") }
      def validPhone = if (pn.matches("[0-9]{10}")) Success(pn) else Failure("Phone Number must be 10 digits")

      map3(validName, validBirthDate, validPhone) { WebForm}
    }
    validWebForm("", "1980-09-05", "12345") shouldEqual Failure("Name cannot be empty", Vector("Phone Number must be 10 digits"))
  }
}
