package part5infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Terminated}
import akka.routing.{ActorRefRoutee, Broadcast, FromConfig, RoundRobinGroup, RoundRobinPool, RoundRobinRoutingLogic, Router}
import com.typesafe.config.ConfigFactory

object Routers extends App {

  /**
   * method 1 - manually created router
   */
  class Master extends Actor {
    // step 1 create routees
    private val slaves = for(i <- 1 to 5) yield {
      val slave = context.actorOf(Props[Slave], s"slave_$i")
      context.watch(slave)

      ActorRefRoutee(slave) // 5 actor routees based off Slave actors
    }

    // step 2 define the router
    private val router = Router(RoundRobinRoutingLogic(), slaves)


    // step 3  route the messages
    override def receive: Receive = {
      // step 4 - handle the termination/ lifecycle of the routees
      case Terminated(ref) =>
        router.removeRoutee(ref)
        val newSlave = context.actorOf(Props[Slave])
        context.watch(newSlave)
        router.addRoutee(newSlave)

      // step 3 - route the message
      case message =>
        router.route(message, sender())
    }
  }

  class Slave extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("RoutersDemo", ConfigFactory.load().getConfig("routerDemo"))
  val master = system.actorOf(Props[Master])

//  for (i <- 1 to 10) {
//    master ! s"[$i] Hello from the world"
//  }

  /**
   * Method 2.1 - a router actor with its own children programmatically
   * POOL router (pool = has its own children)
   */
  val poolMaster = system.actorOf(RoundRobinPool(5).props(Props[Slave]), "simplePoolMaster")
//  for (i <- 1 to 10)  poolMaster ! s"[$i] hello pool master!"

    // method 2.2 configuration
  val poolMaster2 = system.actorOf(FromConfig.props(Props[Slave]), "poolMaster2") // actor name matches the one in application.conf
//  for (i <- 1 to 10)  poolMaster2 ! s"[$i] hello pool master from the world!"

  /**
   * Method #3 - router with actors created elsewhere
   * GROUP router
   */
  // in another part of my application
  val slaveList = (1 to 5).map(i => system.actorOf(Props[Slave], s"slave_$i"))

  // need their path
  val slavePaths = slaveList.map(slaveRef => slaveRef.path.toString)

  val groupMaster = system.actorOf(RoundRobinGroup(slavePaths).props())

  // Method 3.2 from configuration
  // where do we create the routees of this guy?
  val groupMaster2 = system.actorOf(FromConfig.props(Props[Master]), "groupMaster2")
  println(groupMaster2.path)
  groupMaster2 ! "hello"
//  for (i <- 1 to 10)  groupMaster2 ! s"[$i] hello group master 2 from the world!"

  // special messages
  // groupMaster2 ! Broadcast("hello everyone")

}
