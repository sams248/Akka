package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.AkkaCapabilities.Person.LiveALife

object AkkaCapabilities extends App {

  class SimpleActor extends Actor {
    println(context)
    println(context.self)
    println(self)
    println(context.self.path)

    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello there!" // context.sender() (or just sender()) Returns the sender 'ActorRef' of the current message.
      case message: String => println(s"[$self] I have received $message")
      case number: Int => println(s"[simple actor] I have received a number: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something special: $contents!")
      case SendMessageToYourself(content) => self ! content
      case SayHiTo(ref) => ref ! "Hi!"
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s") // by using forward instead of tell, I keep the original sender tof the WirelessPhoneMessage
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")

  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "Hello, actor!"

  /*
   1 - Messages can be of any type, under two conditions:
      (a) messages must be immutable
      (b) messages must be serializable (To serialize an object means to convert its state to a byte stream so that the
          byte stream can be reverted back into a copy of the object. In practice, use case classes and case objects!
   */

  simpleActor ! 88 // who is the sender?
  // see def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit
  // and final val noSender: ActorRef = null

  case class SpecialMessage(contents: String)

  simpleActor ! SpecialMessage("some special content!")

  /*
   2 - Actors have information about their context and about themselves.
       Each actor has a member called context (ActorContext data structure) that has references to information regarding
       the environment the actor runs at, as well as access to the actor's own reference (context.self is the equivalent of `this` in OOP)
   */
  case class SendMessageToYourself(content: String)

  simpleActor ! SendMessageToYourself("I am an actor!")

  /*
   3 - Actors can reply to messages
   */
  val bob = system.actorOf(Props[SimpleActor], "bob")
  val alice = system.actorOf(Props[SimpleActor], "alice")

  case class SayHiTo(ref: ActorRef) // ActorRef is used by Akka to know which actor it should send messages to

  alice ! SayHiTo(bob)

  /*
   4 - Dead letters
   */

  alice ! "Hi!" // reply to "me" (noSender)
  // INFO akka.actor.DeadLetterActorRef - Message [java.lang.String] from Actor[akka://actorCapabilitiesDemo/user/alice#498092718]
  // to Actor[akka://actorCapabilitiesDemo/deadLetters] was not delivered.
  // [1] dead letters encountered. If this is not an expected behavior then Actor[akka://actorCapabilitiesDemo/deadLetters]
  // may have terminated unexpectedly.
  // This logging can be turned off or adjusted with configuration settings 'akka.log-dead-letters' and 'akka.log-dead-letters-during-shutdown'.

  // deadLetters is a fake actor inside Akka that takes care of the messages that are not sent to anyone!

  /*
   5 - Forwarding messages
       Forwarding: Sending a message with the original sender
   */

  case class WirelessPhoneMessage(content: String, ref: ActorRef)

  alice ! WirelessPhoneMessage("Hi", bob) // bob will receive this a distorted message (Hi + s) from the original sender of this message (noSender)

  /**
   * Exercises:
   * 1. Create a counter actor
   *  - Increment
   *  - Decrement
   *  - Print
   *
   * 2. Create a bank account as an actor
   *  - Deposit an amount
   *  - Withdraw an amount
   *  - Statement
   *  - Replies with Success/Failure
   *
   * interact with some other kind of actor
   */

  // 1 counter
  class CounterActor extends Actor {

    import CounterActor._

    var count = 0

    override def receive: Receive = {
      case Increment =>
        println(s"[counter] Incrementing...")
        count += 1
      case Decrement =>
        println(s"[counter] Decrementing...")
        count -= 1
      case Print => println(s"[counter] My current count is $count")
    }
  }

  // Domain of the counter
  object CounterActor {
    case object Increment

    case object Decrement

    case object Print
  }

  val counterActor = system.actorOf(Props[CounterActor], "counterActor")

  import CounterActor._

  (1 to 8).foreach(_ => counterActor ! Increment)
  (1 to 3).foreach(_ => counterActor ! Decrement)
  counterActor ! Print

  // 2 bank account
  object BankAccount {
    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case object Statement

    case class TransactionSuccess(message: String)

    case class TransactionFailure(reason: String)
  }

  class BankAccount extends Actor {

    import BankAccount._

    var funds = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if (amount < 0) sender() ! TransactionFailure("Invalid deposit amount") else {
          funds += amount
          sender() ! TransactionSuccess(s"Successfully deposited $amount")
        }


      case Withdraw(amount) =>
        if (amount < 0) sender() ! TransactionFailure("Invalid withdraw amount") else if (amount > funds) {
          sender() ! TransactionFailure("Insufficient funds")
        } else {
          funds -= amount
          sender() ! TransactionSuccess(s"Successfully withdrew $amount")
        }

      case Statement => sender() ! s"Your balance is $funds"
    }
  }

  object Person {
    case class LiveALife(account: ActorRef)
  }

  class Person extends Actor {

    import Person._
    import BankAccount._

    override def receive: Receive = {
      case LiveALife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }

  val account = system.actorOf(Props[BankAccount], "bankAccount")
  val person = system.actorOf(Props[Person], "sam")

  person ! LiveALife(account)
}
