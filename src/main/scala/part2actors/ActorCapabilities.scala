package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    def receive: Receive = {
      case "hi" => sender ! "Hello, there!"
      case message: String =>
        println(s"[simple actor] I have received $message")
      case number: Int =>
        println(s"[simple actor] I have received a number $number")
      case SpecialMessage(contents) =>
        println(s"[simple actor] I have received something special $contents")
      case SendMessageToYourself(contents) =>
        self ! contents
      case SayHiTo(ref) =>
        ref ! "hi" // will modify the sender because it's the tell method
      case WirelessPhoneMessage(content, ref) =>
        ref forward(content + "s") // will keep the original sender of the previous incovation
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  // 1 - messages can be of any type
  simpleActor ! 42

  case class SpecialMessage(contents: String)
  simpleActor ! SpecialMessage("some special content")

  // you can send any message at all under two conditions:
  // 1: MESSAGES MUST BE IMMUTABLE. No way to detect this at compile time but it is up to you to check.
  // 2: MESSAGES MUST BE SERIALIZABLE - THE JVM CAN MAKE IT INTO A BYTE STRING AND SEND IT TO OTHER JVMs. In practice we
  // use case classes and case objects.

  // actors have information about their context and about themselves
  // Actors have a member called context that have a reference to the actor system and the reference of the actor itself. (self)

  case class SendMessageToYourself (content: String)
  simpleActor ! SendMessageToYourself("I am an actor and proud of it.")

  // 3 - actors can REPLY to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // for every actor at any moment in time context.sender contains the reference of the actor who last sent a message
  // when an actor sends a message to another actor the one sending the message will pass itself as the sender
  // dead letters is a fake actor in akka that takes care to receive the messages not sent to anyone

  // 4 - dead letters
  alice ! "hi"

  // 5 - forwarding - sending a message with the original sender

  case class WirelessPhoneMessage(content: String, ref: ActorRef)
  alice ! WirelessPhoneMessage("hi", bob)
}

