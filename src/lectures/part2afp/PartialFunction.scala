package lectures.part2afp

object PartialFunction extends App {

  val aFunction = (x: Int) => x + 1 // FUNCTION1[Int, Int] === Int => Int
  val aFussyFunction = (x: Int) =>
    if(x == 1) 42
    else if(x == 5) 56
    else if(x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // {1,2,5} => Int

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } //partial function value

//  println(aPartialFunction(3)) - match error

  println(aPartialFunction.isDefinedAt(67))

  // lift
  val lifted = aPartialFunction.lift //Int => Option[Int]
  println(lifted(2))
  println(lifted(4))

  val pfChain = aPartialFunction.orElse[Int, Int]{
    case 45 => 67
  }

  println(pfChain(1))
  println(pfChain(45))

  // PF extend normal functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well

  val aMappedList = List(1,2,3).map {
    case 1 => 42
    case 2=> 78
    case 3 => 1000
  }

  println(aMappedList)

  /*
    Note: PF can only have ONE parameter type
   */

  /*
   * Exercies
   * 1 - constriuct of PF instance yourself (anonymous class)
   * 2 - dumb chatbot as a PF
   */

  val aManualFussyFunction = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 42
      case 2 => 65
      case 5 => 999
    }

    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 5
  }

  val chatBot: PartialFunction[String, String] = {
    case "Hi" => "Hello"
    case "give a name" => "Bob"
    case "how old are you" => "666"
  } //partial function value


  scala.io.Source.stdin.getLines().foreach(line => println(chatBot(line)))
}
