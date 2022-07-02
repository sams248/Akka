package part1recap

import scala.concurrent.Future
import scala.language.implicitConversions

object AdvancedRecap extends App {

  // Partial Functions
  val partialFunction: PartialFunction[Int, Int] = {
    case 1 => 11
    case 2 => 22
    case 3 => 33
  }

  // equivalent to
  val pf = (x: Int) => x match {
    case 1 => 11
    case 2 => 22
    case 3 => 33
  }

  val function: (Int => Int) = partialFunction

  val modifiedList = List(1, 2, 3).map {
    case 1 => 88
    case _ => 0
  }

  // Lifting
  val lifted = partialFunction.lift // turns the partial function into total function Int => Option[Int]
  lifted(2) // Some(22)
  lifted(200) // None

  // orElse
  val pfChain = partialFunction.orElse[Int, Int] {
    case 6 => 66
  }
  pfChain(2) // 22
  pfChain(6) // 66
  // pfChain(444) // throws a MatchError

  // Type aliases
  type ReceiveFunction = PartialFunction[Any, Unit]

  def receive: ReceiveFunction = {
    case 1 => println("Hello")
    case _ => println("Confused!")
  }

  // Implicits

  implicit val timeOut: Int = 3000

  def setTimeout(f: () => Unit)(implicit timeout: Int): Unit = f()

  setTimeout(() => println("timeout")) // extra parameter list omitted

  // implicit conversion
  // 1. implicit defs
  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def fromStringToPerson(string: String): Person = Person(string)

  "Sam".greet // fromStringToPerson("Sam").greet is automatically done by the compiler

  // 2. implicit classes
  implicit class Dog(name: String) {
    def bark(): Unit = println("barking!")
  }

  "Lucy".bark() // new Dog("Lucy").bark() is automatically done by the compiler

  // Organizing implicits
  // local scope
  implicit val inverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  println(List(1, 2, 3, 4).sorted) // List(4,3,2,1,)

  // import scope

  import scala.concurrent.ExecutionContext.Implicits.global

  val future = Future {
    println("Hello future!")
  }

  // companion objects of the types included in the call
  object Person {
    implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }

  println(List(Person("Sam"), Person("Mike")).sorted) // List(Person(Mike), Person(Sam))

}
