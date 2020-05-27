package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChangingActorBehaviorExercises extends App {
  val system = ActorSystem("changingActorBehaviorExercises")

  // exercise 1. recreate the counter actor from a few videos before with become and unbecome, no internal state
  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    import Counter._

    override def receive: Receive = handler(0)

    def handler(count: Int): Receive = {
      case Increment =>
        context.become(handler(count + 1))
      case Decrement =>
        context.become(handler(count - 1))
      case Print =>
        println(count)
    }
  }

  import Counter._
  val counter = system.actorOf(Props[Counter], "myCounter")
  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  /**
   * exercise 2. simplified voting system.
   */
  case class Vote(candidate: String) // send tis to the citizen = mark the citizen as having voted for that and citizen changes the state
  case object VoteStatusRequest
  case class VoteStatusReply(candidate: Option[String])
  class Citizen extends Actor {
    override def receive: Receive = willVote

    def hasVoted(candidate: String): Receive = {
      case VoteStatusRequest =>
        sender ! VoteStatusReply(Some(candidate))
    }

    def willVote: Receive = {
      case Vote(candidate) =>
        context.become(hasVoted(candidate))
      case VoteStatusRequest =>
        sender ! VoteStatusReply(None)
    }
  }

  // the vote aggregator sends messages to the citizen asking them who they have voted for.
  case class AggregateVotes(citizens: Set[ActorRef])
  class VoteAggregator extends Actor {
    override def receive: Receive = waitForCandidates

      def countVotes(voteCounts: Map[String, Int]): Receive = {
        case VoteStatusReply(candidate) => candidate match {
          case Some(name) =>
            val newCounts = addVoteToCounts(voteCounts, name)
            context.become(countVotes(newCounts))
            println(newCounts)
          case None =>
        }
      }

    def waitForCandidates: Receive = {
      case AggregateVotes(citizens) =>
        context.become(countVotes(Map.empty))
        citizens.foreach(citizen => citizen ! VoteStatusRequest)
    }

    def addVoteToCounts(voteCounts: Map[String, Int], aCandidate:  String): Map[String, Int] = {
      val newVoteCountsForCandidate = voteCounts.getOrElse(aCandidate, 0) + 1

      voteCounts + (aCandidate -> newVoteCountsForCandidate)
    }
  }

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val daniel = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))

  /*
  Print the status of the votes

  Map(every candidate -> number of votes)
   */
}
