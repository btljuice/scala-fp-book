package sfpbook.ch11

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.exist.and
import org.scalatest.matchers.should._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfpbook.ch11.Monad.Instances.Id

class MonadSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "optionMonad" should "replicateM generate some" in {
    val m = Monad.Instances.optionMonad
    m.replicateM(5, Some(1)) shouldEqual Some(List.fill(5)(1))
    m.replicateM(5, None) shouldEqual None
  }
  it should "filter values" in {
    val m = Monad.Instances.optionMonad
    val input = List.range(1, 5)
    // Some(true|false) works as a normal filter
    m.filterM(input)(i => if (i % 2 == 0) Some(true) else Some(false)) shouldEqual Some(2 :: 4 :: Nil)
    // If one None is returned, it propagates as the return value
    m.filterM(input)(i => if (i == 4) None else Some(true)) shouldEqual None

  }

  "listMonad" should "replicateM generate all permutations" in {
    val m = Monad.Instances.listMonad
    val input = 1 :: 2 :: 3 :: Nil
    val expected = for { i <- 1 to 3; j <- 1 to 3 } yield i :: j :: Nil
    m.replicateM(2, input) shouldEqual expected.toList
  }
  it should "map2 generate all permutations" in {
    val m = Monad.Instances.listMonad
    val i1 = List.range(1, 4)
    val i2 = List.range(4, 7)
    m.map2(i1, i2) { (_, _) } shouldEqual ( for { i <- i1; j <- i2 } yield (i, j) )
  }
  it should "sequence generate all permutations" in {
    val m = Monad.Instances.listMonad
    val input = List(1 :: 2 :: Nil, 3 :: 4 :: Nil, 5 :: 6 :: Nil)
    m.sequence(input) shouldEqual ( for { i <- input(0); j <- input(1); k <- input(2) } yield i :: j :: k :: Nil )
  }
  it should "filter values w/ all combination" in {
    val m = Monad.Instances.listMonad
    val input = List.range(1, 4)
    val expected = List(
      1 :: 2 :: 3 :: Nil,
      1 :: 2 :: Nil,
      1 :: 3 :: Nil,
      1 :: Nil,
      2 :: 3 :: Nil,
      2 :: Nil,
      3 :: Nil,
      Nil
    )
    m.filterM(input)(_ => List(true, false)) shouldEqual expected
  }

  // FP Book: Monads provide a "context" for introducing and binding variables, and performing variable substitution.
  // Think about map of Option VS map List VS map Either.
  // 1. All do a "substitution", which is "uniform" in terms of the interface to the client:
  //    x.map(f)
  // 2. but handle the specificities for their actual types under-the-hood
  //    Option: only substitutes if prior type is Some(_)
  //    List: substitutes all elements
  //    Either: only substitutes the right-hand side expression
  //    etc.
  "idMonad" should "flatMap be equivalent to get/set" in {
    forAll { (a: Int, b: Int) =>
      val m = Monad.Instances.idMonad
      import m._
      val modified = for { x <- m.unit(a); y <- m.unit(b) } yield { x + y }
      modified.value shouldEqual a + b
    }
  }

  implicit def arbId[A](implicit arb: Arbitrary[A]): Arbitrary[Id[A]] = Arbitrary(arb.arbitrary.map(Id(_)))

  MonadLaw.test[Option, Int, String, Double, Char]("optionMonad", Monad.Instances.optionMonad)
  MonadLaw.test[List, Int, String, Double, Char]("listMonad", Monad.Instances.listMonad)
  MonadLaw.test[Stream, Int, String, Double, Char]("streamMonad", Monad.Instances.streamMonad)
  MonadLaw.test[Id, Int, String, Double, Char]("idMonad", Monad.Instances.idMonad)

  object MonadLaw {
    def test[F[_], A, B, C, D](label: String, m: Monad[F])(implicit
      arbA: Arbitrary[A],
      arbFa: Arbitrary[F[A]],
      arbF: Arbitrary[A => F[B]],
      arbG: Arbitrary[B => F[C]],
      arbH: Arbitrary[C => F[D]],
    ) = {
      import m._
      label + " monad instance" should "be associative" in {
        forAll { (fa: F[A], f: A => F[B], g: B => F[C]) =>
          fa.flatMap(f).flatMap(g) shouldEqual fa.flatMap(a => f(a).flatMap(g))
        }
      }
      it should "be associative 2" in {
        forAll { (a: A, f: A => F[B], g: B => F[C], h: C => F[D]) =>
          m.compose(f, m.compose(g, h))(a) shouldEqual m.compose(m.compose(f, g), h)(a)
        }
      }
      it should "identity return itself" in {
        forAll { fa: F[A] => fa.flatMap(a => m.unit(a)) shouldEqual fa }
        forAll { (a: A, f: A => F[B]) => m.unit(a).flatMap(f) shouldEqual f(a) }
      }
      it should "identity return itself 2" in {
        forAll { (a: A, f: A => F[B]) =>
          m.compose[A, A, B](x => m.unit(x), f)(a) shouldEqual f(a)
          m.compose[A, B, B](f, x => m.unit(x))(a) shouldEqual f(a)
        }
      }
    }
  }
}

// Ex 11.8
// Convert compose to flatMap to show associativity law is equivalent knowing:
// * compose(f, compose(g, h)) = compose(compose(f, g), h)
// * compose(f, g) = a => f(a).flatMap(g)
//
// lhs: a => f(a).flatMap( compose(g, h)       )
//                       ( b=> g(b).flatMap(h) )
//      a => f(a).flatMap( b => g(b).flatMap(h) )
//
// rhs: compose(b => f(b).flatMap(g), h)
//      a => f(a).flatMap(g).flatMap(h)
//
// If f is identity and a == monad then we have the same associativity
//
