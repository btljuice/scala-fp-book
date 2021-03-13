package sfpbook.ch11

import org.scalacheck.Arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.ch11.Monad.Instances.Id
// import sfpbook.ArbitraryDefinitions._ // TODO: Figure out why compilation fails w/ this straight import

class FunctorSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  implicit def arbId[A](implicit arbA: Arbitrary[A]): Arbitrary[Id[A]] = Arbitrary(arbA.arbitrary.map(Id(_)))
  FunctorLaw.test[Option, Int, String, Double, Char]("optionMonad", Monad.Instances.optionMonad)
  FunctorLaw.test[List, Int, String, Double, Char]("listMonad", Monad.Instances.listMonad)
  FunctorLaw.test[Stream, Int, String, Double, Char]("streamMonad", Monad.Instances.streamMonad)
  FunctorLaw.test[Id, Int, String, Double, Char]("idMonad", Monad.Instances.idMonad)

  object FunctorLaw {
    def test[F[_], A, B, C, D](label: String, m: Functor[F])(implicit arbA: Arbitrary[F[A]]) = {
      label + "functor law" should "map + identity return itself" in { forAll { fa: F[A] => m.map(fa)(identity) shouldEqual fa } }
    }
  }
}
