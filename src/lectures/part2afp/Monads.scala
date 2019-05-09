package lectures.part2afp

object Monads extends App {

  trait Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }
  object Attempt {
    def apply[A](a: => A): Attempt[A] = ???
  }
}
