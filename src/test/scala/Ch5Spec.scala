package sfpbook
import org.scalatest._

class Ch5Spec extends FlatSpec with Matchers {
  def integers(n: Int): Stream[Int] = Stream.cons(n, integers(n + 1))
  "Ch5.2" should "take" in {
    integers(0).take(5).toList shouldEqual List(0, 1, 2, 3, 4)
  }
  "Ch5.3" should "takeWhile" in {
    integers(0).takeWhile(_ < 5).toList shouldEqual List(0, 1, 2, 3, 4)
    Stream.empty[Int].takeWhile(_ => true) shouldEqual Stream.empty[Int]
  }
  "Ch5.4" should "forall" in {
    Stream(0, 1, 2, 3, 4, 5).forall(_ <= 4) shouldEqual false
    Stream(0, 1, 2, 3, 4, 5).exists(_ > 4) shouldEqual true
  }
  "Ch5.6" should "headOption" in {
    integers(0).headOption shouldEqual Some(0)
    Stream.empty[Int].headOption shouldEqual None
  }
  "Ch5.7" should "map, filter, flatMap, foldRight" in {
    val s = integers(0).take(5)
    s.map(_ + 1)
      .filter(_ % 2 == 0)
      .flatMap(n => Stream(n, n*2))
      .foldRight(0)(_ + _) shouldEqual 2 + 4 + 4 + 8

  }
  "Ch5.8" should "infinite streams" in {
    Stream.ones.map(_+ 1).exists(_ % 2 == 0) shouldEqual true
    Stream.ones.takeWhile(_ == 1).headOption shouldEqual Some(1)
    Stream.ones.forall(_ != 1) shouldEqual false
    Stream.constant(5).take(3).toList shouldEqual List(5, 5, 5)
    Stream.from(5).take(3).toList shouldEqual List(5, 6, 7)
    Stream.fibs.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }
  "Ch5.14" should "startsWith" in {
    val s = Stream(1, 2, 3, 4, 5)
    s startsWith Stream.empty shouldEqual true
    s startsWith Stream(1, 2, 3) shouldEqual true
    s startsWith Stream(2, 3) shouldEqual false
  }
  "Ch5.15" should "tails" in {
    val s = Stream(1, 2, 3, 4, 5)
    s.tails.map(_.toList).toList shouldEqual List(1, 2, 3, 4, 5) :: List(2, 3, 4, 5) :: List(3, 4, 5) :: List(4, 5) :: List(5) :: Nil
  }
  "Ch5.16" should "hasSubsequence" in {
    val s = Stream(1, 2, 3, 4, 5)
    s.hasSubsequence(Stream(2, 4)) shouldEqual false
    s.hasSubsequence(Stream(2, 3, 5)) shouldEqual false
    Stream(2, 2, 3, 4).hasSubsequence(Stream(2,3)) shouldEqual true
  }
  "Ch5.18" should "scanLeft" in {
    val s = Stream(1, 2, 3, 4, 5)
    s.scanRight(0)(_ + _).toList shouldEqual List(15, 14, 12, 9, 5, 0)
  }
}
