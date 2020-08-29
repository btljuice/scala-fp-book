package sfpbook

//import sfpbook.{Option => FPOption}

import org.scalatest._

class Ch4Spec extends FlatSpec with Matchers {
  "Ch4.1" should "Option correctly" in {
    None.isEmpty shouldEqual true
    an[RuntimeException] should be thrownBy None.get
    Some(4).map(_ + 2) shouldEqual Some(6)
    None.map((i:Int) => i + 2) shouldEqual None
  }
  "Ch4.5" should "traverse correctly" in {
    Option.traverse(List(1, 2, 3))(i => Some(i + 1)) shouldEqual Some(List(2, 3, 4))
    Option.traverse(List(1, 2, 3))(i => if (i == 3) None else Some(i + 1)) shouldEqual None
  }
}
