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
import sfpbook.ch6.State

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
  implicit def arbIntState[A](implicit arb: Arbitrary[Int => (Int, A)]): Arbitrary[State[Int, A]] = Arbitrary(arb.arbitrary.map(f => State[Int, A](f)))
  // TODO: Implement a MonadLaw.testFnc to factor this out and be able to test on all function like types
  "stateMonad" should "identity return itself" in {
    val m = Monad.Instances.stateMonad[Int]
    forAll { (s: String, t: Int, f: String => State[Int, Double]) =>
      m.compose((x: String) => m.unit(x), f)(s)(t) shouldEqual f(s)(t)
      m.compose(f, (x: Double) => m.unit(x))(s)(t) shouldEqual f(s)(t)
    }
  }
  it should "be associative" in {
    val m = Monad.Instances.stateMonad[Int]
    forAll { (s: String, t: Int, f: String => State[Int, Double], g: Double => State[Int, Char], h: Char => State[Int, BigDecimal]) =>
      m.compose(f, m.compose(g, h))(s)(t) shouldEqual m.compose(m.compose(f, g), h)(s)(t)
    }
  }
  it should "map should not modify state" in {
    val m = Monad.Instances.stateMonad[Int]
    forAll { (t0: Int, t1: Int, f: Unit => Double) =>
      State.set(t1).flatMap(u => m.unit(f(u)))(t0) shouldEqual (t1, f(()))
      State.unit[Int].flatMap(u => m.unit(f(u))).get(t0) shouldEqual (t0, t0)
    }
  }

  MonadLaw.test[Option, Int, String, Double, Char]("optionMonad", Monad.Instances.optionMonad)
  MonadLaw.test[List, Int, String, Double, Char]("listMonad", Monad.Instances.listMonad)
  MonadLaw.test[Stream, Int, String, Double, Char]("streamMonad", Monad.Instances.streamMonad)
  MonadLaw.test[Id, Int, String, Double, Char]("idMonad", Monad.Instances.idMonad)
  MonadLaw.test[({type f[x] = Either[String, x]})#f, Int, String, Double, Char]("eitherMonad", Monad.Instances.eitherMonad[String])

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
        forAll { fa: F[A] => fa.flatMap(x => m.unit(x)) shouldEqual fa }
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

// Ex. 12.7
// Given that you have monad. show that applicative laws work as well
// Applicative laws to demonstrate:
//   u = unit(())
//   fst(a,b) = a
//   snd(a,b) = b
//   tup(a,b) = (a, b)
//   x * y = x.map2(y)(tup)
//   1. fa.map2(u)(fst) == fa ; u.map2(fa)(snd) == fa
//   2. fa * (fb * fc) == (fa * fb) * fc map assoc
//   3. fa.map2(fb)((a, b) => (f(a), g(b))) == fa.map(f) * fb.map(g)
//
// Monad laws:
//   1. unit(a).flatMap(f) == f(a) ; f(a).flatMap(unit) == f(a)
//      fa.map(identity) == fa ;
//   2. fa.flatMap(f).flatMap(g) shouldEqual fa.flatMap(a => f(a).flatMap(g))
//
// map2 expressed as flatMap and unit:
//   fa.map2(fb)(f) == fa.flatMap(a => fb.map(b => f(a, b)))
//
// ANSWER:
//   1. fa.map2(u)(fst)
//      fa.flatMap(a => u.map(b => fst(a, b)))
//      fa.flatMap(a => unit(()).map(_ => a))
//                      unit(()).flatMap(unit(a))
//      fa.flatMap(unit)
//      fa
//
//   2. fa * (fb * fc)
//      fa.map2(fb * fc)(tup)
//      fa.map2(fbc)(tup)
//      fa.flatMap(a => fbc.map(bc => tup(a, bc)))
//      fa.flatMap(a => fbc.flatMap(bc => unit . tup(a, bc)))
//      fa.flatMap(_ => fbc)                                   .flatMap(bc => unit . tub(a, bc)))
//      fa.flatMap(a => (fb * fc))                             .
//      fa.flatMap(a => fb.map2(fc)(tup))                      .
//      fa.flatMap(a => fb.flatMap(b => fc.map(c => tup(b, c))).
//      fa.flatMap(_ => fb).flatMap(b => fc.map(c => tub(b, c))).
///     TODO FINISH
///   3. FINISH
