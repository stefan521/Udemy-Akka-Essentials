package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}

object ActorLifecycle extends App {

  object StartChild
  class LifecycleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case StartChild =>
        context.actorOf(Props[LifecycleActor], "child")
    }

    override def preStart(): Unit = {
      log.info("I am starting")
    }

    override def postStop(): Unit = {
      log.info("I have stopped")
    }
  }

  val system = ActorSystem("LifecycleDemo")
  val parent = system.actorOf(Props[LifecycleActor], "parent")
  parent ! StartChild
  parent ! PoisonPill

  /**
   * restart
   */
  object Fail
  object FailChild
  class Parent extends Actor {
    private val child: ActorRef = context.actorOf(Props[Child], "supervisedChild")

    override def receive: Receive = {
      case FailChild => child ! Fail
    }
  }
  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case Fail =>
        log.warning("child will fail now")
        throw new RuntimeException("I failed")
    }

    override def preStart(): Unit = {
      log.info("supervised child started")
    }

    override def postStop(): Unit = {
      log.info("supervised child stopped")
    }

    // called by the old actor instance being replaced
    override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
      log.info(s"supervised actor restarted because of ${reason.getMessage}")
    }

    // called by the new actor instance after it replaces the old one
    override def postRestart(reason: Throwable): Unit = {
      log.info("supervised actor restarted")
    }
  }

  val supervisor = system.actorOf(Props[Parent], "supervisor")
  supervisor ! FailChild

  // as a result of that exception the child restarts by itself
}
