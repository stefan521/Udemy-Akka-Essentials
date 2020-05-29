package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorExercises extends App {
  /**
   *  Exercise 1: Distributed word counting.
   *  2 kinds of actors
   *
   */

  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(id: Int = 0, text: String)
    case class WordCountReply(id: Int, count: Int)
  }
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = handleNonInitialized

    def handleNonInitialized: Receive = {
      case Initialize(nChildren) =>
        val workers = for { i <- 1 to nChildren } yield context.actorOf(Props[WordCounterWorker], s"wcw_$i")
        context.become(handleTasksInitialized(workers, 0, 0, Map.empty))
    }

    def handleTasksInitialized(
      workers: Seq[ActorRef],
      nextActor: Int,
      nextId: Int,
      requester: Map[Int, ActorRef]
    ): Receive = {
      case WordCountTask(id, text) =>
        val worker = workers(nextActor)
        val newRequesters = requester + (id -> sender)
        val nextWorkerIndex = (nextActor + 1) % workers.length
        context.become(handleTasksInitialized(workers, nextWorkerIndex, nextId + 1, newRequesters))
        worker ! WordCountTask(id, text)

      case WordCountReply(id, count) =>
        val newRequesters = requester - id
        println(s"task $id counted $count words")
        context.become(handleTasksInitialized(workers, nextActor, nextId, newRequesters))
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(id, text) =>
        val numberOfWords = text.split(" ").length

        sender ! WordCountReply(id, numberOfWords)
    }
  }

  /*
    flow:
    create a WordCounterMaster
    send Initialize(10) to wordCounterMaster
    send "Akka is awesome" to wordCounterMaster
      wcm will send a WordCountTask("...") to one of its children
        child replies with WordCountReply(3) to the master
       master replies with 3 to the sender.

      requester -> wcm -> wcw
              r <- wcm <-
   */

  import WordCounterMaster._

  val actorSystem = ActorSystem("childActorExercises")
  val master = actorSystem.actorOf(Props[WordCounterMaster], "master")
  master ! Initialize(3)
  master ! WordCountTask(text = "suie paparude pe tramvai in oras")
}
