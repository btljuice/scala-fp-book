package sfpbook

import org.scalatest._

class Ch2Spec extends FlatSpec with Matchers {
  "ch2.ex1.fibtail()" should "give the fibonacci numbers" in {
    ch2.ex1.fibtail(0) shouldEqual 0
    ch2.ex1.fibtail(1) shouldEqual 1
    ch2.ex1.fibtail(2) shouldEqual 1
    ch2.ex1.fibtail(3) shouldEqual 2
    ch2.ex1.fibtail(4) shouldEqual 3
    ch2.ex1.fibtail(5) shouldEqual 5
    ch2.ex1.fibtail(6) shouldEqual 8
    ch2.ex1.fibtail(20) shouldEqual 6765
  }

  "ch2.ex2.isSorted()" should "verify that arrays are sorted" in {
    ch2.ex2.isSorted(Array(1,2,3))(_ < _) shouldEqual true
    ch2.ex2.isSorted(Array(2,1,3))(_ < _) shouldEqual false
    ch2.ex2.isSorted(Array(3,2,1))(_ > _) shouldEqual true
    ch2.ex2.isSorted(Array("A", "B", "C"))(_ < _) shouldEqual true
  }
}
