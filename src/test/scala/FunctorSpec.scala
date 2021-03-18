package sfpbook.ch11

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.ch11.Monad.Instances.Id
import sfpbook.ch12.Applicative
import sfpbook.ch12.Validation
// import sfpbook.ArbitraryDefinitions._ // TODO: Figure out why compilation fails w/ this straight import

class FunctorSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  implicit def arbId[A](implicit arbA: Arbitrary[A]): Arbitrary[Id[A]] = Arbitrary(arbA.arbitrary.map(Id(_)))
  implicit def arbValidation[A](implicit arbA: Arbitrary[A]): Arbitrary[Validation[String, A]] =
    Arbitrary(Gen.oneOf(true, false).flatMap {
      case true => arbA.arbitrary.map(Validation.Success(_))
      case false => Gen.const(Validation.Failure("some error"))
    })


  def testLaws[F[_]](label: String, m: Functor[F])(implicit
    arbA: Arbitrary[F[Int]],
    arbF: Arbitrary[Int => String],
    arbG: Arbitrary[String => Double],
  ) = FunctorLaw.test[F, Int, String, Double](label, m)

  // functor laws
  testLaws("listFunctor", Functor.Instances.listFunctor)
  testLaws("optionFunctor", Functor.Instances.optionFunctor)

  // applicative laws
  testLaws("streamZipApplicative", Applicative.Instances.streamZipApplicative)
//  testLaws("validationApplicative", Applicative.Instances.validationApplicative[String])

  // monad laws
  testLaws("optionMonad", Monad.Instances.optionMonad)
  testLaws("listMonad", Monad.Instances.listMonad)
  testLaws("streamMonad", Monad.Instances.streamMonad)
  testLaws("idMonad", Monad.Instances.idMonad)

  object FunctorLaw {
    def test[F[_], A, B, C](label: String, m: Functor[F])(implicit
      arbA: Arbitrary[F[A]],
      arbF: Arbitrary[A => B],
      arbG: Arbitrary[B => C],
    ) = {
      import m._
      label + " functor law" should "map + identity return itself" in {
        forAll { fa: F[A] => fa.map(identity) shouldEqual fa }
      }
      it should "be associative" in {
        forAll { (fa: F[A], f: A => B, g: B => C) =>
          fa.map(f).map(g) shouldEqual fa.map(g compose f)
        }
      }
    }
  }
}
