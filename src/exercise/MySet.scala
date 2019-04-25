package exercise

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  /*
    EXERCISE- implement a functional set
   */

  override def apply(v1: A): Boolean = contains(v1)

  def contains(elem: A) : Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def -(elem: A): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]

  /*
    EXERCISE
    - remove an element
    - intersection with another set
    - difference with another set
   */

  // new operator
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {

  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
  override def -(elem: A): MySet[A] = this
  override def &(anotherSet: MySet[A]): MySet[A] = this
  override def --(anotherSet: MySet[A]): MySet[A] = this

//  def unary_! : MySet[A] = new MySet[A]
}

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = true

  override def +(elem: A): MySet[A] = this

  override def ++(anotherSet: MySet[A]): MySet[A] = this

  override def map[B](f: A => B): MySet[B] = ???

  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???

  override def filter(predicate: A => Boolean): MySet[A] = ??? // property-based set

  override def foreach(f: A => Unit): Unit = ???

  override def -(elem: A): MySet[A] = ???

  override def &(anotherSet: MySet[A]): MySet[A] = ???

  override def --(anotherSet: MySet[A]): MySet[A] = ???

  override def unary_! : MySet[A] = new EmptySet[A]
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = {
    if (head == elem) tail
    else tail - elem + head
  }

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))


  }


}

object MySet {

  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  // s + 5 ++ MySet(-1,-2) + 3 flatMap (x => MySet(x, x * 10)) filter (_ % 2 == 0) foreach println
  //  s remove 1 foreach println

  val s2 = MySet(0, 1, 3, 5, 8)

  s & s2 foreach println
  println("diff")
  s --  s2 foreach println

}