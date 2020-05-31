package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._

class SynchronousTestingSpec extends WordSpecLike with BeforeAndAfterAll  {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  override def afterAll(): Unit = {
    system.terminate()
  }

  import SynchronousTestingSpec._

  "A counter" should {
    "synchronously increase its counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter ! Inc // SENDING A MESSAGE TO A TestActorRef HAPPENS SYNCHRONOUSLY IN THIS THREAD

      assert(counter.underlyingActor.count == 1)
    }

    "synchronously increase its counter at the receive function" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter.receive(Inc)

      assert(counter.underlyingActor.count == 1)
    }

    "work on the calling thread dispatcher" in {
      val counter = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
      val probe = TestProbe()
      // whatever message we send to this counter will happen on the calling thread

      probe.send(counter, Read) // happens on the Probe's calling thread
      probe.expectMsg(Duration.Zero, 0) // probe has ALREADY received the message 0
    }
  }
}

object SynchronousTestingSpec {
  case object Inc
  case object Read

  class Counter extends Actor {
    var count = 0

    override def receive: Receive = {
      case Inc => count += 1
      case Read => sender ! count
    }
  }
}
