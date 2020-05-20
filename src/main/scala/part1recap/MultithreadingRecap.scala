package part1recap

import scala.concurrent.Future
import scala.util.{Failure, Success}

object MultithreadingRecap extends App {

  // creating a threads on the JVM
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("I'm running in parallel")
  })

  // or with syntax sugar
  val anotherThread = new Thread(() => println("I'm running in parallel"))

  aThread.start()
  aThread.join()

  val threadHello = new Thread(() => (1 to 1000).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 1000).foreach(_ => println("goodbye")))

  threadHello.start()
  threadGoodbye.start()

  // but with threads different runs produce different results. they're very unpredictable

  // volatile only works for primitive types. it makes the value "atomic"
  class BankAccount(@volatile private var amount: Int) {
    override def toString: String = amount.toString

    def withdraw(money: Int): Unit = this.amount -= money

    def safeWithdraw(money: Int): Unit = this.synchronized {
      this.amount -= money
    }
  }

  // inter-thread communication on the JVM is done via the wait - notify mechanism

  // Scala Futures
  import scala.concurrent.ExecutionContext.Implicits.global
  val future = Future {
    // long computation - on a different thread
    42
  }

  future.onComplete {
    case Success(42) => println("I found the meaning of life")
    case Failure(_) => println("something happened with the meaning of life!")
  }

  val aProcessedFuture = future.map(_ + 1) // Future with 43
  val aFlatFuture = future.flatMap { value =>
    Future(value + 2) // Future with 44
  }
  val filteredFuture = future.filter(_ % 2 == 0) // this might fail with NoSuchElementException
  val aNonsenseFuture = for {
    meaningOfLife <- future
    filteredMeaning <- filteredFuture
  } yield meaningOfLife + filteredMeaning

  // andThen, recover/ recoverWith
}
