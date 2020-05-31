package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}
import akka.pattern.{ ask, pipe }
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class AskSpec extends TestKit(
  ActorSystem("AskSpec")
) with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import AskSpec._

  "An authenticator" should {
    import AuthManager._

    "fail to authenticate a non-registered user" in {
      val authManager = system.actorOf(Props[AuthManager])

      authManager ! Authenticate("Daniel", "rockTheJvm")

      expectMsg(AuthFailure(AUTH_FAILURE_NOT_FOUND))
    }

    "fail to authenticate if invalid password" in {
      val authManager = system.actorOf(Props[AuthManager])
      authManager ! RegisterUser("Daniel", "dumbPassword")

      authManager ! Authenticate("Daniel", "dumbERpassword")

      expectMsg(AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT))

    }
  }

}

object AskSpec {

  // this code is somewhere else in your app
  case class Read(key: String)
  case class Write(key: String, value: String)
  class KVActor extends Actor with ActorLogging {
    override def receive: Receive = online(Map.empty)

    def online(kv: Map[String, String]): Receive = {
      case Read(key) =>
        log.info(s"Trying to read the value at the key $key")
        sender ! kv.get(key) // Option[String]
      case Write(key, value) =>
        log.info(s"Writing the value $value for the key $key")
        context.become(online(kv + (key -> value)))
    }
  }

  // user authentication actor
  case class RegisterUser(username: String, password: String)
  case class Authenticate(username: String, password: String)
  case class AuthFailure(message: String)
  case object AuthSuccess
  class AuthManager extends Actor with ActorLogging {
    import AuthManager._

    val authDb = context.actorOf(Props[KVActor])
    // import akka.pattern.ask
    // you need implicit values for the future that ask returns
    implicit val timeout: Timeout = Timeout(1 second)
    implicit val executionContext: ExecutionContext = context.dispatcher

    implicit
    override def receive: Receive = {
      case RegisterUser(username, password) =>
        authDb ! Write(username, password)
      case Authenticate(username, password) =>
       handleAuthenticate(username, password)
    }

    def handleAuthenticate(username: String, password : String): Unit = {
      val originalSender = sender
      val future = authDb ? Read(username)
      future.onComplete {
        // step 5 most important - never call methods on the actor instance or access mutable state in onComplete
        // avoid closing over the actor instance or mutable state
        case Success(None) => originalSender ! AuthFailure(AUTH_FAILURE_NOT_FOUND)
        case Success(Some(dbPassword)) =>
          if (dbPassword == password) originalSender ! AuthSuccess
          else originalSender ! AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
        case Failure(_) => originalSender ! AuthFailure(AUTH_FAILURE_SYSTEM)
      }
    }
  }

  object AuthManager {
    val AUTH_FAILURE_NOT_FOUND = "username not found"
    val AUTH_FAILURE_PASSWORD_INCORRECT = "password incorrect"
    val AUTH_FAILURE_SYSTEM = "system error"
  }

  class PipedAuthManager extends AuthManager {
    import AuthManager._

    override def handleAuthenticate(username: String, password: String): Unit = {
      val future = authDb ? Read(username) // Future[Any]
      val passwordFuture = future.mapTo[Option[String]] // Future[Option[String]]
      val responseFuture = passwordFuture.map {
        case None => AuthFailure(AUTH_FAILURE_NOT_FOUND)
        case Some(dbPassword) =>
          if (dbPassword == password) AuthSuccess
          else AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
      } // FUTURE[AuthSuccess/ AuthFailure] but for the compilet this is Future[Any]

      /**
       * When the future completes, it will send whatever it contains to this actor ref you pass in the arg list
       */
      responseFuture.pipeTo(sender)
    }
  }
}

// never call methods or access actor states in callbacks
