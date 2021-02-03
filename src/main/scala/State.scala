package sfpbook.ch6

case class State[T, +A](run: T => (T, A)) {
  type S[B] = State[T, B]
  final def apply(t: T): (T, A) = run(t)
  final def map[B](f: A => B): S[B] = flatMap { a => State.value(f(a)) }
  final def flatMap[B](f: A => S[B]): S[B] = { t: T =>
    val (t1, a) = run(t)
    f(a)(t1)
  }
  final def map2[B, C](sb: => S[B])(f: (A, B) => C): S[C] =
    for { a <- this ; b <- sb } yield { f(a, b) }
}
object State {
  implicit def state[T, A](f: T => (T, A)): State[T, A] = State(f)

  final def get[T]: State[T, T] = { t: T => (t, t) }
  final def set[T](t: => T): State[T, Unit] = { _: T => (t, ()) }
  final def modify[T](f: T => T): State[T, Unit] = get.flatMap(t => set(f(t)))

  def unit[T]: State[T, Unit] = value(())
  def value[T, A](a: => A): State[T, A] = { t: T => (t, a) }
  def both[T, A, B](a: State[T, A], b: State[T, B]): State[T, (A, B)] = a.map2(b)((_, _))
  def sequence[T, A](s: List[State[T, A]]): State[T, List[A]] = s match {
    case Nil => value(Nil)
    case a :: t => a.map2(sequence(t)) { _ :: _ }
  }
}
