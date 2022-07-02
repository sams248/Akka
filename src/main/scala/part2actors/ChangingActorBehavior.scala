package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehavior.Mom.MomStart

object ChangingActorBehavior extends App {

  object FussyKid {
    case object KidAccept

    case object KidReject

    val HAPPY = "happy"
    val SAD = "sad"
  }

  class FussyKid extends Actor {

    import FussyKid._
    import Mom._

    // internal state of the kid
    var state: String = HAPPY

    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) =>
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {

    import FussyKid._
    import Mom._

    override def receive: Receive = happyReceive // by default

    def happyReceive: Receive = {
      // become() changes the Actor's behavior to become the new 'Receive' handler.
      case Food(VEGETABLE) => context.become(sadReceive) // change my receive handler to sadReceive
      case Food(CHOCOLATE) => // stay happy
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(CHOCOLATE) => context.become(happyReceive) // change my receive handler to sadReceive
      case Food(VEGETABLE) => // stay sad
      case Ask(_) => sender() ! KidReject

    }
  }

  object Mom {
    case class MomStart(kidRef: ActorRef)

    case class Food(foot: String)

    case class Ask(message: String)

    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"

  }

  class Mom extends Actor {

    import Mom._
    import FussyKid._

    override def receive: Receive = {
      case MomStart(kidRef) =>
        // test our interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Ask("Do you want to play?")
      case KidAccept => println("Yay! My kid is happy!")
      case KidReject => println("My kid is sad, but at least he's healthy!")
    }
  }

  val system = ActorSystem("changingActorBehaviorDemo")
  val fussyKid = system.actorOf(Props[FussyKid])
  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid])
  val mom = system.actorOf(Props[Mom])

  // mom ! MomStart(fussyKid)
  mom ! MomStart(fussyKid)

  /*
   become() can take a second parameter, discardOld (Boolean). It will then act upon the behavior stack as follows:
     if discardOld = true it will replace the top element (i.e. the current behavior)
     if discardOld = false it will keep the current behavior and push the given one atop

   With become() without the second parameter, or if it is set to true:
     Food(veg) -> message handler turns to sadReceive
     Food(chocolate) -> message handler turns to happyReceive

   With become() and second parameter set to false:
   Initial stack:
    1. happyReceive
   Food(veg) -> stack.push(sadReceive)
   Stack:
     1. sadReceive
     2. happyReceive
   Food(chocolate) -> stack.push(happyReceive)
   Stack:
     1. happyReceive
     2. sadReceive
     3. happyReceive

   Akka always call the top-most handler in the stack, if stack is empty, akka will call the plain receive method

   context.unbecome() pops a handler from the top of stack. It takes no parameters
 */

  /**
   * Exercise 1:
   * Recreate the Counter actor with context.become() and not mutable state
   */

  // Takeaway: If you want to re-write a stateful actor to a stateless actor, you need to rewrite its mutable state into
  // the parameters of the receive handlers that you want to support

  class Counter extends Actor {

    import Counter._

    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment =>
        println(s"[countReceive($currentCount)] incrementing...")
        context.become(countReceive(currentCount + 1))
      case Decrement =>
        println(s"[countReceive($currentCount)] decrementing...")
        context.become(countReceive(currentCount - 1))
      case Print => println(s"[countReceive($currentCount)] My current count is $currentCount")
    }
  }

  // Domain of the counter
  object Counter {
    case object Increment

    case object Decrement

    case object Print
  }

  val counter = system.actorOf(Props[Counter], "counter")

  import Counter._

  (1 to 8).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  /**
   * Exercise 2
   * Simplified voting system
   * Use stateful actors first and then refactor into stateless actors
   */

  case class Vote(candidate: String)

  case object VoteStatusRequest

  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {
    // var candidate: Option[String] = None

    override def receive: Receive = {
      case Vote(c) => context.become(voted(c)) //candidate = Some(c)
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }

    def voted(candidate: String): Receive = {
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
    }
  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoterAggregator extends Actor {
    //    var stillWaiting: Set[ActorRef] = Set()
    //    var currentStats: Map[String, Int] = Map()
    //
    //    override def receive: Receive = {
    //      case AggregateVotes(citizens) =>
    //        stillWaiting = citizens
    //        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
    //      case VoteStatusReply(None) =>
    //        // a citizen has not voted yet
    //        sender() ! VoteStatusRequest // this might end uo in an infinite loop, but in our example we'll make sure all the citizens vote
    //      case VoteStatusReply(Some(candidate)) =>
    //        val newStillWaiting = stillWaiting - sender()
    //        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
    //        currentStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
    //        if (newStillWaiting.isEmpty) println(s"[aggregator] poll stats: $currentStats")
    //        else {
    //          stillWaiting = newStillWaiting
    //        }
    //    }

    override def receive: Receive = awaitingCommand

    def awaitingCommand: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
        context.become(awaitingStatuses(citizens, Map()))
    }

    def awaitingStatuses(stillWaiting: Set[ActorRef], currentStats: Map[String, Int]): Receive = {
      case VoteStatusReply(None) =>
        // a citizen has not voted yet
        sender() ! VoteStatusRequest
      case VoteStatusReply(Some(candidate)) =>
        val newStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
        val newStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
        if (newStillWaiting.isEmpty) println(s"[aggregator] poll stats: $newStats")
        else {
          // still need to process some statuses
          context.become(awaitingStatuses(newStillWaiting, newStats))
        }
    }
  }

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val sam = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  sam ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoterAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, sam))

  /*
   Print the status of the votes:
     Martin -> 1
     Jonas -> 1
     Roland -> 2
   */
}
