package part1recap

import scala.annotation.tailrec
import scala.util.Try

object GeneralRecap extends App {

  // Value
  val aBoolean: Boolean = false
  var aVariable: Int = 88
  aVariable += 1 // aVariable is now 89

  // Expressions
  val anIfExpression: String = if (2 > 3) "bigger" else "smaller"

  // Instructions vs expressions
  val theUnit: Unit = println("Hello Scala!") // Unit === "void" in Java

  def aFunction(x: Int): Int = x + 1

  // Recursion - tail-recursion
  @tailrec
  def factorial(n: Int, acc: Int): Int = {
    if (n <= 0) acc else factorial(n - 1, acc * n)
  }

  // Object-Oriented Programming
  class Animal

  class Dog extends Animal

  val aDog = new Dog

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // Inheritance: extends <= 1 class, but inherit from >= 0 traits
  class Crocodile extends Animal with Carnivore {
    // We have to implement everything that is abstract in both Animal and Carnivore
    override def eat(animal: Animal): Unit = println("eating!")
  }

  // Method notations
  val aCrocodile = new Crocodile
  aCrocodile.eat(aDog)
  aCrocodile eat aDog

  // Anonymous classes
  val aCarnivore: Carnivore = new Carnivore {
    override def eat(animal: Animal): Unit = println("roaring")
  }

  aCarnivore eat aDog

  // Generics
  abstract class MyList[+A]

  // Companion objects
  object MyList

  // Case classes
  case class Person(name: String, age: Int)

  // Exceptions
  val aPotentialFailure: Unit = try {
    throw new RuntimeException("I am innocent!") // Nothing, not even null or unit
  } catch {
    case e: Exception => println("I caught an exception!")
  } finally {
    // Side effects
    println("Some logs")
  }

  // Functional Programming
  val incrementer: Int => Int = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val incremented: Int = incrementer(44) // under the hood incrementer is an instance of a class, for us it is just a function!
  // incrementer.apply(44)

  val anonymousIncrementer: Int => Int = (x: Int) => x + 1
  // syntax sugar for Function1[Int, Int]

  // FP is all about working with functions as first-class
  List(1, 2, 3).map(incrementer) // map is higher-order-function

  // for-comprehensions
  val pairs: Seq[String] = for {
    num <- List(1, 2, 3, 4)
    char <- List("a", "b", "c", "d")
  } yield num + "-" + char

  // Equivalent to:
  List(1, 2, 3, 4).flatMap(num => List('a', 'b', 'c', 'd').map(char => num + "-" + char))

  // Seq, Array, List, Vector, Map, Tuples, Sets
  // other "collections" : Option and Try
  val anOption = Some(2)
  val doubleOption = anOption.map(_ * 2)

  val anAttempt: Try[Int] = Try(12)
  val modifiedAttempt = anAttempt.map(_ * 10)
  val aTry = Try {
    throw new RuntimeException
  }

  // Pattern matching
  val anUnknown: Any = 45
  val medal = anUnknown match {
    case 1 => "gold"
    case 2 => "silver"
    case 3 => "bronze"
    case _ => "no medal"
  }

  val sam = Person("Sam", 25)
  val greeting = sam match {
    case Person(n, _) => s"Hi, my name is $n"
    case _ => "I don't know my name!"
  }
}
