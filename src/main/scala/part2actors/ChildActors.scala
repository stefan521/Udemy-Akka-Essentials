package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChildActors.CreditCard.AttachToAccount

object ChildActors extends App {
  // Actors can create other actors using the actor context

  object Parent {
    case class CreateChild(name: String)
    case class TellChild(message: String)
  }
  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        // create an actor right here
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(childRef: ActorRef): Receive = {
      case TellChild(msg) =>
        if (childRef != null) childRef forward msg
    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got: $message")
    }
  }

  import Parent._
  val system = ActorSystem("ParentChildDemo")
  val parent = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")
  parent ! TellChild("hey kid")

  // this creates actor hierarchies
  // parent -> child -> grandchild
  //        -> child2

  /** someone owns parent as well (top-level actors/ guardian actors)
   *
   * there are 3 guardian actors.
   * /system = system guardian
   * /user = user-level guardian - this guy owns all of our actors
   * / = the root guardian - manages both the system guardian and the user guardian.
   * the root guardian sits at the root of the actor system itself.
   *
   */

  /**
   * Actor selection below
   *
   * a wrapper over a potential actor ref.
   */
  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you!"

  /**
   *
   * DANGER !!!!
   *
   * NEVER PASS MUTABLE ACTOR STATE OR THE `THIS` REFERENCE TO CHILD ACTORS
   *
   * DANGER !!! NEVER !!!
   *
   * this will break actor encapsulation.
   *
   */
  object NaiveBankAccount {
    case class Deposit(amount: Int)
    case class WithDraw(amount: Int)
    case object InitializeAccount
  }
  class NaiveBankAccount extends Actor {
    import NaiveBankAccount._
    import CreditCard._

    var amount = 0

    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        creditCardRef ! AttachToAccount(this) // we are all doomed
      case Deposit(funds) => deposit(funds)
      case WithDraw(funds) => withdraw(funds)
    }

    def deposit(funds: Int): Unit = {
      println(s"${self.path} depositing $funds on top of $amount")
      amount += funds
    }
    def withdraw(funds: Int): Unit = {
      println(s"${self.path} withdrawing $funds from the $amount")
      amount -= funds
    }
  }

  object CreditCard {
    case class AttachToAccount(bankAccount: NaiveBankAccount) // what the hell?
    case object CheckStatus
  }
  class CreditCard extends Actor {
    override def receive: Receive = {
      case AttachToAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case checkStatus =>
        println(s"${self.path} your message has been processed.")
        account.withdraw(1) // absolutely bananas
    }
  }

  import NaiveBankAccount._
  import CreditCard._

  val bankAccountRef = system.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)

  Thread.sleep(500)
  val ccSelection = system.actorSelection("/user/account/card")
  ccSelection ! CheckStatus

  // conclusion = we never call methods on actors we send messages instead
  // never close over (pass to other stuff) mutable state or the `this` reference
}
