package part2actors

import akka.actor.{Actor, ActorRef, ActorSelection, ActorSystem, Props}
import part2actors.ChildActors.Parent.{CreateChild, TellChild}

object ChildActors extends App {

  // Actors can create other actors
  object Parent {
    case class CreateChild(name: String)

    case class TellChild(message: String)
  }

  class Parent extends Actor {

    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child...")
        // create a new actor here
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(childRef: ActorRef): Receive = {
      case TellChild(message) =>
        childRef forward message
    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got: $message")
    }
  }

  val system: ActorSystem = ActorSystem("ParentChildDemo")
  val parent: ActorRef = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")
  parent ! TellChild("hey kid!")

  // Actor hierarchies:
  //  parent -> child -> grandChild
  //         -> child2 ->

  /*
   Guardian (top-level) actors:
    - /system = system guardian: Akka or other libraries built on top of Akka may create actors in the system namespace.
    - /user = user-level guardian: This is the top level actor that you provide to start all other actors in your application.
    - / = the root guardian: This is the parent of all actors in the system, and the last one to stop when the system itself is terminated.
   */

  /**
   * Actor selection
   */
  val childSelection = system.actorSelection("/user/parent/child") // locate an actor using a path
  childSelection ! "I found you!"

  /**
   * Danger!
   * Never pass mutable actor state, or the `this` reference, to child actors!
   * This can break actor encapsulation.
   */

  object NaiveBankAccount {
    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case object InitializeAccount
  }

  class NaiveBankAccount extends Actor {

    import NaiveBankAccount._
    import CreditCard._

    var amount = 0

    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        creditCardRef ! AttachToAccount(this) // !!
      case Deposit(funds) => deposit(funds)
      case Withdraw(funds) => withdraw(funds)
    }

    def deposit(funds: Int): Unit = {
      println(s"${self.path} depositing $funds on top of $amount.")
      amount += funds
    }

    def withdraw(funds: Int): Unit = {
      println(s"${self.path} withdrawing $funds from $amount.")
      amount -= funds
    }
  }

  object CreditCard {
    case class AttachToAccount(bankAccount: NaiveBankAccount) // !! Instead of actor reference, this message contains an
    // instance of an actor JVM object. It should be AttachToAccount(bankAccountRef: ActorRef)
    // Every single interaction with actors must happen through messages

    case object CheckStatus
  }

  class CreditCard extends Actor {

    import CreditCard._

    override def receive: Receive = {
      case AttachToAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case CheckStatus => println(s"${self.path} your message has been processed.")
        // benign
        account.withdraw(1) // because I can and that is a problem!! Very hard to debug!
        // We should never call a method directly on actors, we should only send messages
    }
  }

  import NaiveBankAccount._
  import CreditCard._

  val bankAccountRef: ActorRef = system.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)

  Thread.sleep(500)
  val creditCardSelection: ActorSelection = system.actorSelection("/user/account/card")
  creditCardSelection ! CheckStatus // will withdraw money!

}
