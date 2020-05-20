package part1recap

import scala.util.Try

object GeneralRecap extends App {

  val aCondition: Boolean = false

  var aVariable = 42
  aVariable += 1

  // expressions
  val aConditionedVal = if (aCondition) 42 else 65

  // a code block (still an expression). the result of a code block its last expression
  val aCodeBlock = {
    if (aCondition) 74
    else 56
  }

  // types
  // Unit - used for return values of functions with side effects. all side effects functions return Unit
  val theUnit: Unit = println("Hello, Scala")

  // functions
  def aFunction(x: Int) = x + 1

  // normal recursion  = STACK recursion
  // recursion - TAIL recursion
  @scala.annotation.tailrec
  def factorial(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorial(n - 1, acc * n)

  // OOP
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit // unimplemented = abstract
  }

  class Crocodile extends Animal with  Carnivore {
    override def eat(a: Animal): Unit = println("crunch")
  }

  // method notation
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // infix method notation

  // anonymous classes - instantiate traits or abstract classes => new anonymous class
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit =  println("roar")
  }

  // generics
  abstract class MyList[+A]
  // companion objects
  object MyList

  // case classes
  case class Person(name: String, age: Int) // a LOT in this course!

  // Exceptions
  val aPotentiaonFailure = try {
    throw new RuntimeException("I'm innocent, I swear!") // Nothing - not even null, not even Unit. returns nothing
  } catch {
    case e: Exception => "I caught an exception!"
  } finally {
    println("some logs")
  }

  // Functional Programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val incremented = incrementer(42)
  // incrementer.apply(42)

  val anonymousIncrementer = (x: Int) => x + 1
  // type of Int => Int == Function1[Int, Int]

  // FP is all about working with functions as first-class
  List(1, 2, 3).map(incrementer)
  // map = Higher Order Function because it takes a function as a parameter


  // for comprehensions
  val pairs = for {
    num <- List(1, 2, 3, 4)
    char <- List('a', 'b', 'c', 'd')
  } yield num + "-" + char

  //translates to what a scala compiler desugars a for comprehension to
  List(1, 2, 3, 4).flatMap(num => List('a', 'b', 'c', 'd').map(char => num + "-" + char))

  // collections Seq, Array, List, Vector, Map, Tuples, Sets

  // other collections Option and Try
  val anOption = Some(2)
  val aTry = Try {
    throw new RuntimeException
  }

  // pattern matching
  val unknown = 2
  val order = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  var bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
    case _ => "I don't know my name"
  }
}
