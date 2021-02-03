package sfpbook.ch3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.tailrec

class Ch3Spec extends AnyFlatSpec with Matchers {
  lazy val l = List(1, 2, 3, 4, 5)
  lazy val l1 = List(6, 7, 8, 9, 10)
  "ch3.7" should "Compute product correctly" in {
    l.foldRight(1)(_ * _) shouldEqual 120
    l.foldRight(0)(_ * _) shouldEqual 0
  }
  "ch3.8" should "l.foldRight(Nil)(Cons) should be equivalent to identity" in {
    val result = l.foldRight(List.empty[Int])((i, x) => Cons(i, x))
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
    l.reverse shouldEqual List(5, 4, 3, 2, 1)
  }
  "ch3.14" should "append correctly" in {
    l ::: Nil shouldEqual l
    l ::: l1 shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }
  "ch3.15" should "flatten list" in {
    def flatten(ls: List[List[Int]]): List[Int] = ls.foldRight(Nil: List[Int]) { (z, h) => h ::: z }
    flatten(List(l, l1, l)) shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5)
  }
  "ch3.16" should "map correctly" in {
    l.map(_ + 1) shouldEqual List(2, 3, 4, 5, 6)
    l.map(_.toString) shouldEqual List("1", "2", "3", "4", "5")
  }
  "ch3.17" should "filter correctly" in {
    l.filter(_ % 2 == 0) shouldEqual List(2, 4)
  }
  "ch3.18" should "flatMap correctly" in {
    l.flatMap(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  }
  "ch3.19" should "zipWith correctly" in {
    l.zipWith(l1){ _ + _ } shouldEqual List(7, 9, 11, 13, 15)
  }

  "ch3.23" should "drop/take correctly" in {
    l.drop(3) shouldEqual List(4, 5)
    l.dropWhile(_ < 4) shouldEqual List(4, 5)
    l.take(2) shouldEqual List(1, 2)
    l.takeWhile(_ < 4) shouldEqual List(1, 2, 3)
  }
  "ch3.23+1" should "forall/exists correctly" in {
    l.forall(_ <= 5) shouldEqual true
    l.exists(_ == 3) shouldEqual true
  }
  "ch3.24" should "hasSubsequence correctly" in {
    @tailrec def startsWith[T](l: List[T], sub: List[T]): Boolean = (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && startsWith(t1, t2)
    }
    def hasSubSequence[T](l: List[T], sub: List[T]): Boolean = l match {
      case Nil => sub.isEmpty
      case Cons(_, t1) => startsWith(l, sub) || startsWith(t1, sub)
    }

    hasSubSequence(List(1, 2, 3, 4), List(2, 4)) shouldEqual false
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
