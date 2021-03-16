package sfpbook.ch11

import org.scalacheck.Arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.ch11.Monad.Instances.Id
// import sfpbook.ArbitraryDefinitions._ // TODO: Figure out why compilation fails w/ this straight import

class FunctorSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  implicit def arbId[A](implicit arbA: Arbitrary[A]): Arbitrary[Id[A]] = Arbitrary(arbA.arbitrary.map(Id(_)))
  FunctorLaw.test[Option, Int, String, Double, BigDecimal]("optionMonad", Monad.Instances.optionMonad)
  FunctorLaw.test[List, Int, String, Double, BigDecimal]("listMonad", Monad.Instances.listMonad)
  FunctorLaw.test[Stream, Int, String, Double, BigDecimal]("streamMonad", Monad.Instances.streamMonad)
  FunctorLaw.test[Id, Int, String, Double, BigDecimal]("idMonad", Monad.Instances.idMonad)

  object FunctorLaw {
    def test[F[_], A, B, C, D](label: String, m: Functor[F])(implicit
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
