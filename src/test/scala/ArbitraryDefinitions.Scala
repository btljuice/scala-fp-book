package sfpbook

import org.scalacheck.Arbitrary
import sfpbook.ch11.Monad.Instances.Id

object ArbitraryDefinitions {
  implicit def arbId[A](implicit arbA: Arbitrary[A]): Arbitrary[Id[A]] = Arbitrary(arbA.arbitrary.map(Id(_)))
}
