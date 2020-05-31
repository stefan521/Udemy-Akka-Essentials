package part5infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, PoisonPill, Props, Timers}

import scala.concurrent.duration._

object TimersSchedulers extends App {

  class SimpleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("SchedulersTimersDemo")
  val simpleActor = system.actorOf(Props[SimpleActor])

  system.log.info("Scheduling reminder for simple actor")
  system.scheduler.scheduleOnce(1 second) {
//    simpleActor ! "reminder"
  } (system.dispatcher) // system.dispatcher is an execution context

  val routine: Cancellable = system.scheduler.schedule(1 second, 2 seconds) {
//    simpleActor ! "heartbeat"
  } (system.dispatcher)

  system.scheduler.scheduleOnce(5 seconds ) {
//    routine.cancel
  } (system.dispatcher)

  /**
   * things to bear in mind. don't use unstable references
   *
   * all scheduled tasks execute when the system is terminated
   *
   * schedulers are not very precise and can't be used long-term
   */

  /**
   *  Exercise
   *  - if the actor receives a message you have 1 second to send it another message
   *  - if the time window expires, the actor will stop itself
   *  - if you send another message, the time window is reset
   */
  class ClosingActor extends Actor with ActorLogging {
    override def receive: Receive = notStarted

    def notStarted: Receive = {
      case message =>
        log.info(s"started with $message")
        context.become(waitingForMessage(willShutDown()))
    }

    def waitingForMessage(selfShutdown: Cancellable): Receive = {
      case message =>
        selfShutdown.cancel
        log.info(s"I got another message $message")
        context.become(waitingForMessage(willShutDown()))
    }

    def willShutDown(): Cancellable = {
      context.system.scheduler.scheduleOnce(1 second)  {
        log.info("Shutting down")
        self ! PoisonPill
      } (system.dispatcher)
    }
  }

  val selfClosingActor = system.actorOf(Props[ClosingActor], "waitingActor")

  val sendMessageToWaitingActor: Cancellable = system.scheduler.schedule(initialDelay = Duration.Zero, interval = 0.5 seconds) {
//    selfClosingActor ! "heartbeat"
  } (system.dispatcher)

  system.scheduler.scheduleOnce(5 seconds ) {
//    sendMessageToWaitingActor.cancel
  } (system.dispatcher)

  //timers
  case object TimerKey // ONE KEY PER TIMER
  case object Start
  case object Reminder
  case object Stop
  class TimerBasedHeartBeatActor extends Actor
    with ActorLogging
    with Timers {
    timers.startSingleTimer(TimerKey, Start, 500 millis)

    override def receive: Receive = {
      case Start =>
        log.info("Bootstrapping")
        timers.startPeriodicTimer(TimerKey, Reminder, 1 second) // the previous timer is cancelled cause we used the same key
      case Reminder =>
        log.info("I am alive")
      case Stop =>
        log.warning("Stopping")
        timers.cancel(TimerKey)
        context.stop(self)
    }
  }

  val timerHearthBeatActor = system.actorOf(Props[TimerBasedHeartBeatActor], "heartbeatActor")
  system.scheduler.scheduleOnce(5 seconds) {
    timerHearthBeatActor ! Stop
  } (system.dispatcher)
}
