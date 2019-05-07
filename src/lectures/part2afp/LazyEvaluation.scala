package lectures.part2afp

object LazyEvaluation extends App {

  // lazy DELAYES the evaluation of values
  lazy val x: Int =
  {
    println("hello")
    42
  }
  println(x)
  println(x)

  // examples of implications
  // side effects
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyVCondition = sideEffectCondition
  println(if (simpleCondition && lazyVCondition) "yes" else "no")

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = {
    lazy val t = n // only evaluated once
    t + t + t + 1
  }
  def retrieveMagicValue = {
    println("waiting")
    Thread.sleep(1000)
    42
  }
  println(byNameMethod(retrieveMagicValue))
  // use lazy vals

  // filtering with lazy vals
  def lessThat30(i: Int): Boolean = {
    println(s"$i is less that 30?")
    i < 30
  }

  def greaterThat20(i: Int): Boolean = {
    println(s"$i is greater that 20?")
    i > 20
  }

  val numbers = List(1,25,40,5,23)
  val lt30 = numbers.filter(lessThat30) // List(1,25,5,23)
  val gt20 = lt30.filter(greaterThat20) // List(25,23)

  println(gt20)

  val lt30lazy = numbers.withFilter(lessThat30) // lazy va;ls under the hood
  val gt20lazy = lt30lazy.withFilter(greaterThat20)

  println
  gt20lazy.foreach(println)


  // for-comprehensions use withFilter with guards
  for {
    a <- List(1,2,3) if a % 2 == 0 // use lazy vals
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1) // LList[Int]

  /*
    Exercise: implement a lazily evaluated, singly linked STREAM of elements

    naturals = MyStream.from(1)(x => x + 1) - INFINITE!
    naturals.take(100).foreach(println)
    naturals.map(_ * 2 ) // stream of all even numbers (infinite)
   */



  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] // prepand operator
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] // concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]
    def takeAsList(n: Int): List[A]
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = ???
  }
}
