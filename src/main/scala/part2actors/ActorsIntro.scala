package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {
  // part1 - actor systems

  // heavy weight data structure controlling a number of thread. recommended to only have 1 actor system
  // actor system must contain only alphanumeric characters and -
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // actors are uniquely identified
  // messages are asynchronous
  // each actor may respond differently
  // actors are very encapsulated

  //part 2 - create actors
  // word count actor

  class WordCountActor extends Actor {
    // internal data
    var totalWords = 0

    // behavior or receive handler that akka will invoke
    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // instantiate the actor - can't do it with new. you have to use the actor system
  // often a good idea to name actors
  // now we have an actor reference which means we can only communicate with the actor via messages. we can't poke inside it
  // actor names must be unique
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordActor")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  // part4 - communicate! the exclamation mark is the method we are invoking in infix notation
  // completely asynchronous
  wordCounter ! "I am learning Akka and it's pretty damn cool!"
  anotherWordCounter ! "A different message"

  // actorOf is a factory method
  // the exclamation method is also known as "tell"
  // type Receive = PartialFunction[Any, Unit]


  // instantiating subclasses of Actor
  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _ =>
    }
  }

  object Person {
    def props(name: String): Props = Props(new Person(name))
  }
  
  // this is legal but this is DISCOURAGED!
  val person = actorSystem.actorOf(Person.props("Bob"))
  person ! "hi"
}
