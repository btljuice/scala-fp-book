package sfpbook

//import sfpbook.{Option => FPOption}

import org.scalatest._

class Ch4Spec extends FlatSpec with Matchers {
  "Ch4.1" should "Option correctly" in {
    None.isEmpty shouldEqual true
    an[RuntimeException] should be thrownBy None.get
    Some(4).map(_ + 2) shouldEqual Some(6)
    None.map((i:Int) => i + 2) shouldEqual None
    None.flatMap((i:Int) => Some(i +2)) shouldEqual None
    Some(4).flatMap(i => Some(i + 2)) shouldEqual Some(6)
    Some(4).flatMap(i => None) shouldEqual None
    None.filter(_ => true) shouldEqual None
    Some(5).filter(_ => false) shouldEqual None
    Some(5).filter(_ == 5) shouldEqual Some(5)
    Some(5).filter(_ == 4) shouldEqual None
  }
  "Ch4.5" should "traverse correctly" in {
    Option.traverse(List(1, 2, 3))(i => Some(i + 1)) shouldEqual Some(List(2, 3, 4))
    Option.traverse(List(1, 2, 3))(i => if (i == 3) None else Some(i + 1)) shouldEqual None
  }
}
