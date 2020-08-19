package sfpbook

import org.scalatest._

class Ch3Spec extends FlatSpec with Matchers {
  lazy val l = Ch3.List(1, 2, 3, 4, 5)
  lazy val l1 = Ch3.List(6, 7, 8, 9, 10)
  "ch3.7" should "Compute product correctly" in {
    l.foldRight(1)(_ * _) shouldEqual 120
    l.foldRight(0)(_ * _) shouldEqual 0
  }
  "ch3.8" should "l.foldRight(Nil)(Cons) should be equivalent to identity" in {
    val result = l.foldRight[Ch3.List[Int]](Ch3.Nil)((x, i) => Ch3.Cons(i, x))
    result shouldEqual l
  }
  "ch3.9" should "l.length == 5" in {
    l.length shouldEqual 5
  }
  "ch3.11" should "sum and product correctly" in {
    l.foldLeft(0)(_ + _) shouldEqual 15
    l.foldLeft(1)(_ * _) shouldEqual 120
  }
  "ch3.12" should "reverse correctly" in {
    l.reverse shouldEqual Ch3.List(5, 4, 3, 2, 1)
  }
  "ch3.14" should "append correctly" in {
    l ::: Ch3.Nil shouldEqual l
    l ::: l1 shouldEqual Ch3.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }
  "ch3.15" should "flatten list" in {
    def flatten(ls: Ch3.List[Ch3.List[Int]]): Ch3.List[Int] = ls.foldRight(Ch3.Nil: Ch3.List[Int]) { (z, h) => h ::: z }
    flatten(Ch3.List(l, l1, l)) shouldEqual Ch3.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5)
  }
  "ch3.16" should "map correctly" in {
    l.map(_ + 1) shouldEqual Ch3.List(2, 3, 4, 5, 6)
    l.map(_.toString) shouldEqual Ch3.List("1", "2", "3", "4", "5")
  }
  "ch3.17" should "filter correctly" in {
    l.filter(_ % 2 == 0) shouldEqual Ch3.List(2, 4)
  }
  "ch3.18" should "flatMap correctly" in {
    l.flatMap(i => Ch3.List(i, i)) shouldEqual Ch3.List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  }
  "ch3.19" should "zipWith correctly" in {
    l.zipWith(l1){ _ + _ } shouldEqual Ch3.List(7, 9, 11, 13, 15)
  }

  "ch3.23" should "drop/take correctly" in {
    l.drop(3) shouldEqual Ch3.List(4, 5)
    l.dropWhile(_ < 4) shouldEqual Ch3.List(4, 5)
    l.take(2) shouldEqual Ch3.List(1, 2)
    l.takeWhile(_ < 4) shouldEqual Ch3.List(1, 2, 3)
  }
  "ch3.23+1" should "forall/exists correctly" in {
    l.forall(_ <= 5) shouldEqual true
    l.exists(_ == 3) shouldEqual true
  }
  "ch3.24" should "hasSubsequence correctly" in {
    def hasSubSequence[T](l: List[T], sub: List[T]): Boolean = (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (h1 :: t1, h2 :: t2) if h1 == h2 => hasSubSequence(t1, t2) || hasSubSequence(t1, sub)
      case (_ :: t1, _) => hasSubSequence(t1, sub)
    }

    hasSubSequence(List(1, 2, 3, 4), List(2, 3, 5)) shouldEqual false
    hasSubSequence(List(2, 2, 3, 4), List(2, 3)) shouldEqual true
  }

  lazy val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4),Leaf(3)))
  lazy val t1 = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(5),Leaf(4)))
  "ch3.25" should "length correctly" in {
    t.length shouldEqual 7
  }
  "ch3.26" should "max correctly" in {
    t.fold(identity)(Math.max) shouldEqual 4
  }
  "ch3.27" should "depth correctly" in {
    t.depth shouldEqual 3
  }
  "ch3.27" should "map correctly" in {
    lazy val tPlus1 = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(5),Leaf(4)))
    t.map(_ + 1) shouldEqual tPlus1
  }
}
