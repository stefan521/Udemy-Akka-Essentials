package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.util.{Failure, Success, Try}

object ActorBasicsExercises extends App {

  /**
   *  What Daniel did differently:
   *  He put the relevant case classes for receive inside a companion object and called that the "DOMAIN"
   *  Then you import that whenever you need it.
   *
   *  It your case class doesn't need any parameter you can make a case object:
   *
   *  case object Increment
   *  case object Decrement
   */

  /**
   * 1. a Counter actor
   *    - Increment counter
   *    - Decrement counter
   *    - Print counter value
   *
   * 2. a Bank account as an actor
   *    - Deposit an amount
   *    - Withdraw an amount
   *    - Statement
   *    - this guy will reply with a success or a failure of the operations
   *    - interacts with some other kind of actor
   *
   */

  class CounterActor extends Actor {
    var counter = 0

    def receive: Receive = {
      case "increment" =>
        counter += 1
      case "decrement" =>
        counter -= 1
      case "print" =>
        println(counter)
    }
  }

  class BankAccount extends Actor {
    var balance = 0

    def depositMoney(amount: Int): TransactionOutcome = {
      balance += amount

      TransactionOutcome(Success(balance))
    }

    def withdrawMoney(amount: Int): TransactionOutcome = {
      if (balance - amount < 0) {
        val reason = new IllegalArgumentException(s"insufficient funds to withdraw $amount")
        TransactionOutcome(Failure(reason))
      } else {
        balance -= amount
        TransactionOutcome(Success(balance))
      }
    }

    def receive: Receive = {
      case Deposit(amount) =>
        val result = depositMoney(amount)
        sender ! result
      case Withdraw(amount) =>
        sender ! withdrawMoney(amount)
      case unknown =>
        println(s"[Bank Account] Unknown operation $unknown")
    }
  }

  object ATM {
    def props(bankAccount: ActorRef): Props = Props(new ATM(bankAccount))
  }

  class ATM(bankAccount: ActorRef) extends Actor {
    def receive: Receive = {
      case Deposit(amount) =>
        bankAccount ! Deposit(amount)
      case Withdraw(amount) =>
        bankAccount ! Withdraw(amount)
      case TransactionOutcome(outcome) =>
        println(s"[ATM Actor] $outcome")
    }
  }

  case class Deposit(amount: Int)
  case class Withdraw(amount: Int)
  case class TransactionOutcome(outcome: Try[Int])

  val actorSystem = ActorSystem("exercises-actor-system")
  val counterActor = actorSystem.actorOf(Props[CounterActor], "counterActor")

  counterActor ! "increment"
  counterActor ! "increment"
  counterActor ! "decrement"
  counterActor ! "print"

  val bankAccountActor = actorSystem.actorOf(Props[BankAccount], "bankAccountActor")
  val atmActor = actorSystem.actorOf(ATM.props(bankAccountActor), "atmActor")

  atmActor ! Withdraw(100)
  atmActor ! Deposit(350)
  atmActor ! Withdraw(150)
}
