package example

object Hello extends Greeting with App {

  def mixinExample() {
    abstract class A {
      val message: String
    }
    class B extends A {
      val message = "I'm an instance of class B"
    }
    trait C extends A {
      def loudMessage = message.toUpperCase()
    }
    class D extends B with C

    val d = new D
    println(d.message)  // I'm an instance of class B
    println(d.loudMessage)  // I'M AN INSTANCE OF CLASS B
  }

  def richIteratorExample() {
    abstract class AbsIterator {
      type T
      def hasNext: Boolean
      def next(): T
    }
    class StringIterator(s: String) extends AbsIterator {
      type T = Char
      private var i = 0
      def hasNext = i < s.length
      def next() = {
        val ch = s charAt i
        i += 1
        ch
      }
    }
    trait RichIterator extends AbsIterator {
      def foreach(f: T => Unit): Unit = while (hasNext) f(next())
    }
    class RichStringIter extends StringIterator("Scala") with RichIterator
    val richStringIter = new RichStringIter
    richStringIter foreach println
  }

  def caseClass() {
    case class Book(isbn: String)
    val frankenstein = Book("978-0486282114")
  }

  def listOfAnyObjects() {
    val list: List[Any] = List(
      "a string",
      732,  // an integer
      'c',  // a character
      true, // a boolean value
      () => "an anonymous function returning a string"
    )
    list.foreach(element => println(element))
  }

  def NodeExample() {
    trait Node[+B] {
      def prepend[A >: B](elem: A): Node[A]
    }

    case class ListNode[+B](h: B, t: Node[B]) extends Node[B] {
      def prepend[A >: B](elem: A): ListNode[A] = ListNode(elem, this)
      def head: B = h
      def tail: Node[B] = t
    }

    case class Nil[+B]() extends Node[B] {
      def prepend[A >: B](elem: A): ListNode[A] = ListNode(elem, this)
    }

    class S;
    class T extends S;


    var l : Node[T] = Nil[T]()
    var l1 : Node[S] = l.prepend(new S)
  }

  class ContainsObject(val aValue: String) {
    object AObject {
      val value = aValue
    }
  }

  def classContainsObjectExample() {
    val a = new ContainsObject("a")
    val b = new ContainsObject("b")

    println(s"a.AObject.value = ${a.AObject.value}")
    println(s"b.AObject.value = ${b.AObject.value}")
  }

  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
