package sfpbook

import scala.annotation.tailrec

object Ch2 {
  object ex1 {
    // ex. 2.1
    def fib(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }

    def fibtail(n: Int): Int = {
      @tailrec def fib(i: Int, f1: Int, f2: Int): Int = {
        require(0 <= i && i <= n)
        if (i == n) f2 else fib(i + 1, f2, f1 + f2)
      }
      if (n <= 0) 0 else fib(1, 0, 1)
    }
  }

  object ex2 {
    @tailrec def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
      (as.length <= 1) || ordered(as(0), as(1)) && isSorted(as.tail)(ordered)
    }
  }

  object ex3 {
    def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
  }
  object ex4 {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  }
  object ex5 {
    def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  }
}
