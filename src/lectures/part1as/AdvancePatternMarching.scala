package lectures.part1as

object AdvancePatternMarching extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }

  /*
    - constans
    - wilcards
    - case classes
    - tuples
    - some special magic like above
   */

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(arg: Person): Option[(String, Int)] =
      if(arg.age < 21) None
      else Some((arg.name, arg.age))

    def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 30)

  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
  }
  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }
  println(legalStatus)

  /*
    Exercise.
   */

  val n: Int = 46
  val mathProperty = n match {
    case x if x < 10 => "single digit "
    case x if x % 2 == 0 => "an even number"
    case _ => "no property"
  }

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10
  }
  val myMathProperty = n match {
    case singleDigit() => "single digit "
    case even() => "an even number"
    case _ => "no property"
  }

  println(myMathProperty)

  // infix patterns
  case class Or[A, B](a: A, b: B)
  val either = Or(2, "two")

  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)


  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if(list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "Starting with 1, 2"
    case _ => "something else"
  }

  println(decomposed)

  // custom return types for unapply
//  isEmpty: Boolean, get:something

  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T
  }

    object PersonWrapper {
      def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
        override def isEmpty: Boolean = false
        override def get: String = person.name
      }
    }

    println(bob match {
      case PersonWrapper(n) => s"This person's name is $n"
    })
}
