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
}
