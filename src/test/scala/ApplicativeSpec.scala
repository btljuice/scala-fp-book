package sfpbook.ch12

import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.Date
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.util.Try
import sfpbook.ch11.Monad
import sfpbook.ch11.Monad.Instances.Id

class ApplicativeSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  // TODO Move this into ArbitraryDefinitions
  implicit def arbId[A](implicit arbA: Arbitrary[A]): Arbitrary[Id[A]] = Arbitrary(arbA.arbitrary.map(Id(_)))
  implicit def arbValidation[A](implicit arbA: Arbitrary[A]): Arbitrary[Validation[String, A]] =
    Arbitrary(Gen.oneOf(true, false).flatMap {
      case true => arbA.arbitrary.map(Validation.Success(_))
      case false => Gen.const(Validation.Failure("some error"))
    })

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

  testLaws("streamZipApplicative", Applicative.Instances.streamZipApplicative)
//  testLaws("validationApplicative", Applicative.Instances.validationApplicative[String])

  // monad laws
  testLaws("optionMonad", Monad.Instances.optionMonad)
  testLaws("listMonad", Monad.Instances.listMonad)
  testLaws("streamMonad", Monad.Instances.streamMonad)
  testLaws("idMonad", Monad.Instances.idMonad)

  def testLaws[F[_]](label: String, m: Applicative[F])(implicit
    arbA: Arbitrary[F[Int]],
    arbB: Arbitrary[F[String]],
    arbC: Arbitrary[F[Double]],
  ) = ApplicativeLaw.test[F, Int, String, Double, BigDecimal](label, m)

  object ApplicativeLaw {
    def test[F[_], A, B, C, D](label: String, a: Applicative[F])(implicit
      arbA: Arbitrary[F[A]],
      arbB: Arbitrary[F[B]],
      arbC: Arbitrary[F[C]],
      arbF: Arbitrary[A => C],
      arbG: Arbitrary[B => D],
    ) = {
      import a._
      label + " applicative law" should "map2 + unit should return itself" in {
        forAll { fa: F[A] =>
          map2(fa, unit(()))((a, _) => a) shouldEqual fa
          map2(unit(()), fa)((_, a) => a) shouldEqual fa
        }
      }
      it should "map3 be associative" in {
        def assoc[A, B, C](t: ((A, B), C)) = t match { case ((a, b), c) => (a, (b, c)) }
        forAll { (fa: F[A], fb: F[B], fc: F[C]) =>
          product(fa, product(fb, fc)) shouldEqual product(product(fa, fb), fc).map(assoc)
        }
      }
      // Applying f and g before OR after product creation are equivalent
      it should "respect naturality of product" in {
        forAll { (fa: F[A], fb: F[B], f: A => C, g: B => D) =>
          map2(fa, fb) { (a, b) => (f(a), g(b)) } shouldEqual product(fa.map(f), fb.map(g))
        }
      }
    }
  }
}
