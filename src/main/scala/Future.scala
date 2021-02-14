package sfpbook.ch7

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable
import scala.collection.immutable.PagedSeq
import scala.collection.mutable
import scala.concurrent.duration.TimeUnit
import scala.util.Failure
import scala.util.Success
import scala.util.Try

// * Goal of the library: Do arbitrary computation in parallel
// * This could look like a function
//     def parMap[A, B](ss: Seq[A])(f: A => B): Future[Seq[B]]
//     val outputList = Future.await { parMap(inputList)(f) }
// * Assumption: No side-effect

// 1st idea:
// * The usual sequential aggregation of values goes through a _.foldLeft method
//    ex. Aggregation sum values
//       > List(1, 2, 3,).foldLeft(0)(_ + _)
// * Assuming
//   - Order of aggregation is not important
//   - The aggregation operation is a costy operation, and we could save time by computing it in parallel.
// * We could use a divide-and-conquer approach, to speed up the process. (See. def sum below)


// * Bad API example:
/* trait Runnable { def run: Unit }
   class Thread(r: Runnable) {
     def start: Unit
     def join: Unit
    }
 */
// 1. In order for Runnable to return a value, there must be a side effect involved as all api method return Unit.
//    This is bad for composability.
// 2. Thread maps directly to the "User/OS Thread".
//    We would ideally want to separate the concern of how task are dispatched in parallel to the implementation
//    and probably parametrizable in some way.

// * Less Bad API example:
/* class ExecutorService {
     def submit[A](a: Callable[A]): Future[A]
   }
   trait Future[A] {
     def get: A
   }
 */
// 1. Too low of an abstraction.
// 2. Future.get is blocking.
// 3. No way for composing Future (main caveat)
//    - This will be our goal

object Par {
  private trait Callable[A] { def call: A }
  abstract class ExecutorService { def submit[A](a: Callable[A]): Future[A] }
  sealed trait Future[A] { private[Par] def apply(k: A => Unit): Unit }

  private def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call = r })
  private def future[A](f: (Try[A] => Unit) => Unit): Future[A] = new Future[A] { override def apply(cb: Try[A] => Unit): Unit = f(cb) }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => future { _(Try(a)) }
  def fork[A](pa: => Par[A]): Par[A] = es => future { cb => eval(es){
    Try {
      pa(es)(cb)
    } match {
      case f: Failure[A] => cb(f)
      case _: Success[A] => ()
    } } }
  def delay[A](pa: => Par[A]): Par[A] = es => pa(es)

  implicit class RichPar[A](pa: Par[A]) {
    def run(implicit es: ExecutorService): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      pa(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      ref.get
    }
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = es => future { cb =>
      var ar: Option[Try[A]] = None
      var br: Option[Try[B]] = None
      val combiner = Actor[Either[Try[A],Try[B]]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(cb(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(cb(f(a, b)))
        }
      }
      pa(es)(a => combiner ! Left(a))
      pb(es)(b => combiner ! Right(b))
    }

    def map[B](f: A => B): Par[B] = map2(unit(())) { (a, _) => f(a) }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a)) // Derived from the former 2. Composability gives choice to the library user
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case pa :: pt => pa.map2(sequence(pt)) { _ :: _ }
  }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = sequence(as.map(asyncF(f)))
  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = parMap(as){ Some(_).filter(p) }.map(_.flatten)
  def parReduce[A, B](as: IndexedSeq[A])(f: (A, A) => A): Par[A] = {
    require(as.nonEmpty, "Must be non-empty")
    if (as.size <= 1) unit(as(0))
    else if (as.size == 2) lazyUnit(f(as(0), as(1)))
    else {
      val (l, r) = as.splitAt(as.size / 2)
      fork(parReduce(l)(f)).map2 { fork(parReduce(r)(f)) }(f)
    }
  }

  // Law of mapping
  // 1. Identity law.
  //    map(unit(x))(f) == unit(f(x))
  //    map(unit(x))(id) == unit(x)
  //    map(y)(id) = y
  // 2. Composability law
  //    map( map(unit(x))(g) )(f)
  // == map( unit(g(x)) )(f)
  // == unit(f.g(x))
  // == map(unit(x))(f.g)
  //
  // Law of forking
  // fork(x) == x   // Forking should alter the result of the computation, but simply calculate it asynchronously
}


  private object Design {
    /* Library choice:
     Q1: Should evaluation:
        Option 1: not start until Par.get() gets called?
                  if so, then in the example below, the code won't be running in parallel, due to the "strict"
                  evaluation of scala
        OR
        Option 2: Should Par.unit() start parallel execution immediately?
                  if so, then Par.unit() is not referential transparent related to Par.unit you can verify this by
                  inlining `sumL` and `sumR` below.

     In this chapter, we'll choose the option 2.
     - Because of that, it is better to compose futures together and delay "Par.get" until the last moment,
     as it is a sync point.
   */
    def sum1[T](ss: IndexedSeq[T])(implicit n: Numeric[T]): T = {
      if (ss.size <= 1) ss.headOption getOrElse n.zero
      else {
        val (l, r) = ss.splitAt(ss.size / 2)
        val sumL = Par.unit(sum1(l))
        val sumR = Par.unit(sum1(r))
        n.plus(Par.get(sumL), Par.get(sumR))
      }
    }

    /* Second attempt at sum. We've designed a map2 signature.
       Q2: Should Par.map2 parameters be lazily or stricly evalutated ?
           Option 1: strictly evaluated.
                     - if so, then the left side computation `a` is recursively spawned before the right side
                       computation `b`. This implies that the left side computation will start evaluating even before
                       the right side start to be constructed.
                     - It's probably a bad design choice to insert a left-bias in our map2 function, as it will probably
                       be our building block for `def sequence` later
            Option 2: lazily evaluated.
                      - if the parameters are lazy evaluated is combined w/ a strict construction, of the parallel
                        computation description, then the resulting object/data holding the description may be too heavy
                        even undermining the actual parallelism performance enhancement.
                      - Also there's the question of when does that description starts to be executed.
            Solution: choose lazy evaluation of the parameters combined w/ lazy description construction.
                      Goal: Don't introduce any bias
                            Don't hold an heavy object
     */
    def sum2[T](ss: IndexedSeq[T])(implicit n: Numeric[T]): Par[T] = {
      if (ss.size <= 1) Par.unit(ss.headOption getOrElse n.zero)
      else {
        val (l, r) = ss.splitAt(ss.size / 2)
        Par.map2(sum2(l), sum2(r))(n.plus)
      }
    }

    /*
    Par.fork is introduced to explicitly decide when to do an asynchronous computation. We leave the choice to the user
    to decide whether a computation should be done a/synchronously on another/main thread.
     */
    def sum3[T](ss: IndexedSeq[T])(implicit n: Numeric[T]): Par[T] = {
      if (ss.size <= 1) Par.unit(ss.headOption getOrElse n.zero)
      else {
        val (l, r) = ss.splitAt(ss.size / 2)
        map2(fork(sum3(l)), fork(sum3(r)))(n.plus)
      }
    }
  }
}



