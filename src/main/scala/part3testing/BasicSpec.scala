package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._
import scala.util.Random

// testActor is the guy that is passed implicitly as the sender of the test messages thanks to our ImplicitSender mixin

class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import BasicSpec._

  "A simple actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "hello, test"
      echoActor ! message

      expectMsg(message) // akka.test.single-expect-default
    }
  }

  "A black hole actor" should {
    "send back some message" in {
      val blackHole = system.actorOf(Props[BlackHole])
      val message = "hello, test"
      blackHole ! message

      expectNoMessage(1 second)
    }
  }

  // message assertions
  "A lab test actor" should {
    val labTestActor = system.actorOf(Props[LabTestActor])

    "turn a string into uppercase" in {
      labTestActor ! "I love Akka"
      val reply = expectMsgType[String] // get the message and play around with it

      assert(reply === "I LOVE AKKA")
    }

    "reply to a greeting" in {
      labTestActor ! "greeting"

      expectMsgAnyOf("hi", "hello")
    }

    "reply with favorite tech" in {
      labTestActor ! "favoriteTech"

      expectMsgAllOf("scala", "akka")
    }

    "reply with cool tech in a different way" in {
      labTestActor ! "favoriteTech"

      val messages = receiveN(2) // returns a Seq[Any]

      // free to do more complicated assertions
    }

    "reply with cool tech in a fancy way" in {
      labTestActor ! "favoriteTech"

      expectMsgPF() {
        case "scala" =>
        case "akka" =>
      }
    }
  }

}

object BasicSpec {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender ! message
    }
  }

  class BlackHole extends Actor {
    override def receive: Receive = {
      case message =>
    }
  }

  class LabTestActor extends Actor {
    val random = new Random()

    override def receive: Receive = {
      case "greeting" =>
        if (random.nextBoolean) sender ! "hi" else sender ! "hello"

      case "favoriteTech" =>
        sender ! "scala"
        sender ! "akka"

      case msg : String => sender ! msg.toUpperCase
    }
  }
}
