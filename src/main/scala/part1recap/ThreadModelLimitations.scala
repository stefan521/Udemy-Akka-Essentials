package part1recap

import scala.concurrent.Future

object ThreadModelLimitations extends App {
  /*
    Daniel's rants
   */

  /**
   *  DR #1 OOP encapsulation is only valid in the SINGLE THREADED MODEL.
   */
  class BankAccount(private var amount: Int) {
    override def toString: String = amount.toString

    def withdraw(money: Int): Unit = this.synchronized {
      this.amount -= money
    }

    def deposit(money: Int): Unit = this.synchronized {
      this.amount += money
    }

    def getAmount: Int = amount
  }

//  val account = new BankAccount(2000)
//  for (_ <- 1 to 1000) {
//    new Thread(() => account.withdraw(1)).start()
//  }
//
//  for (_ <- 1 to 1000) {
//    new Thread(() => account.deposit(1)).start()
//  }
//  println(account.getAmount)

  // locks solve an issue but introduce a baziilion other like deadlock, livelocks, complicatedness


  /**
   * DR #2: delegating something to a thread is a pain
   */
  // you have a running thread and you want to pass a runnable to that thread.

  var task: Runnable = _

  val runningTread: Thread = new Thread(() => {
    while (true) {
      while (task == null) {
        runningTread.synchronized {
          println("[background] waiting for a task... ")
          runningTread.wait()
        }
      }

      task.synchronized {
        println("[background] I have a task!")
        task.run()
        task = null
      }
    }
  })

  def delegateToBackgroundThread(r: Runnable): Unit = {
    if (task == null) task = r

    runningTread.synchronized {
      runningTread.notify()
    }
  }

  runningTread.start()
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println(42))
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println("this should run in the background"))

  /**
   * DR #3: verrrry hard to trace and deal with errors in a multithreaded environment
   */
  // 1M numbers in between 10 threads
  import scala.concurrent.ExecutionContext.Implicits.global

  val futures = (0 to 9)
    .map(i => 100000 * i until 100000 * (i + 1))
    .map(range => Future {
      if (range.contains(521901)) throw new RuntimeException("invalid number")
      range.sum
    })

  val sumFuture = Future.reduceLeft(futures)(_ + _)
  sumFuture.onComplete(println)
}
