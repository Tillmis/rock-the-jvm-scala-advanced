package lectures.part2afp

object CurriesPAF extends App {

  //curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5))

  println(superAdder(3)(5))

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y

  val add4: Int => Int = curriedAdder(4)

  // lifting = ETA-EXPANSION

  // functions != methods (JVM limitations)

  def inc(x: Int) = x + 1

  List(1, 2, 3).map(inc) // ETA-expansion
  List(1, 2, 3).map(x => inc(x))

  // Partial function applications
  val add5 = curriedAdder(5) _ // Int => Int

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  //add7
  val add7_1 = (x: Int) => simpleAddFunction(x, 7)
  val add7_2 = (x: Int) => simpleAddMethod(x, 7)
  val add7_3 = curriedAddMethod(7) _ // PAF
  val add7_4 = simpleAddFunction.curried(7)
  val add7_5 = curriedAddMethod(7)(_) // PAF
  val add7_6 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
  val add7_7 = simpleAddFunction(7, _: Int)

  println(add7_1(3))
  println(add7_2(3))
  println(add7_3(3))

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?" )
  println(insertName("Pawel"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String)
  println(fillInTheBlanks("Pawel", " Scala is awesome!"))

  // EXERCISES
  /*
    1. Process a list of numbers and return their string representations with different formats
       Use the %4.2f, %8.6f and %14.12f with a curried formatter function
   */

  def formatter(format: String)(n: Double) = format.format(n)
  val for_1 = formatter("%4.2f") _
  val for_2 = formatter("%8.6f") _
  val for_3 = formatter("%14.12f") _

  List(1.2,2.6,3.99).map(for_1).foreach(println)

  /*
    2. difference between
     - functions vs methods
     - parameters: by-name vs lambda
   */
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  /*
    calling byName and byFunction
      - int
      - method
      - parenMethod
      - lambda
      - PAF
   */

  byName(23)
  byName(method)
  byName(parenMethod())
  byName(parenMethod) // ok but beware ==> byName(parenMethod())
  byName((() => 42)())
//  byName(parenMethod _) NOK

//  byFunction(42) NOK
//  byFunction(method)  NOK !!!!!!!!! does not do ETA-expansion
  byFunction(parenMethod) // OK compiler does ETA-expansion
  byFunction(() => 42)
  byFunction(parenMethod _) // also works, but warning - unnecessary
}