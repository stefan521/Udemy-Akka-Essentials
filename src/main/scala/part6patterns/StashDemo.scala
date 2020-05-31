package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

object StashDemo extends App {

  /**
   * ResourceActor
   * open => it can receive read/ write requests to the resource
   * otherwise it will postpone all read/ write requests until state is open
   *
   * ResourceActor is closed
   *  Open => switch to the open state
   *  Read, Write messages are POSTPONED
   *
   * ResourceActor is open
   *  Read, Write are handled
   *  Close => switch to the closed state
   *
   */
  case object Open
  case object Close
  case object Read
  case class Write(data: String)

  // step 1 - mix in the stash trait
  class ResourceActor extends Actor with ActorLogging with Stash {
    private var innerData: String = ""

    override def receive: Receive = closed

    def closed: Receive = {
      case Open =>
        log.info("Opening resource")
        unstashAll() // unstash messages when you can handle them
        context.become(open)

      case message =>
        log.info(s"Stashing $message because I can't handle it in the closed state")
        stash() // step 2 - stash away messages that you cannot handle
    }

    def open: Receive = {
      case Read =>
        log.info(s"I have read the $innerData")
      case Write(data) =>
        log.info(s"I am writing $data")
        innerData = data
      case Close =>
        log.info("Closing resource")
        unstashAll()
        context.become(closed)
      case message =>
        log.info(s"Stashing $message because I can't handle it in the open state")
        stash()
    }
  }

  val system = ActorSystem("StashDemo")
  val resourceActor = system.actorOf(Props[ResourceActor])

  resourceActor ! Read // stashed
  resourceActor ! Open // handled switch to open pop READ
  resourceActor ! Open // stash
  resourceActor ! Write("I love stash") // handled
  resourceActor ! Close // handled become closed pop OPEN -> handled OPEN switch to OPEN again
  resourceActor ! Read // stashed

  // potential memory limits
  // make sure you mix in the Stash trait last

}
