package part5infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

object Dispatchers extends App {

  class Counter extends Actor with ActorLogging {
    var count = 0

    override def receive: Receive = {
      case message =>
        count += 1
        log.info(s"[$count] $message")
    }
  }

  val system = ActorSystem("DispatchersDemo") // ConfigFactory.load().getConfig("dispatchersDemo")
  val actors = for (i <- 1 to 10) yield
    system.actorOf(Props[Counter].withDispatcher("my-dispatcher"), s"counter_$i")

  val r = new Random()
//  for (i <- 1 to 1000) {
//    actors(r.nextInt(10)) ! i
//  }

  // method 2 - from config
  val rtjvmActor = system.actorOf(Props[Counter], "rtjvm")

  /**
   * Dispatchers implement the execution context trait
   */
  class DBActor extends Actor with ActorLogging {
    // not good to use context.dispatcher cause that's normally used for dispatching messages
    private implicit val executionContext: ExecutionContext = context.dispatcher

    override def receive: Receive = {
      case message => Future {
        // wait on a resource
        Thread.sleep(5000)
        log.info(s"Success: $message")
      }
    }
  }

  val dbActor = system.actorOf(Props[DBActor], "dbActor")
  dbActor ! "the meaning of life is 42"
}
