package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.util.Random

class BasicSpec extends TestKit(ActorSystem("BasicSpec")) // instantiates this actor system when run
  with ImplicitSender // sends testActor as the implicit sender of all the messages
  with AnyWordSpecLike // a trait that allows the description of the tests in a natural English style
  with BeforeAndAfterAll { // supplies some hooks that will be called when test suite is called

  // setup
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import BasicSpec._

  // this is a test in out test suite
  "A simple actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "Hello, test!"
      echoActor ! message

      // Receive one message from the test actor and assert that it equals the given object.
      expectMsg(message) // akka.test.single-expect-default
    }
  }

  "A black hole actor" should {
    "not send back any message" in {
      val blackHoleActor = system.actorOf(Props[BlackHole])
      val message = "Hello, test!"
      blackHoleActor ! message

      expectNoMessage(1.second)
    }
  }

  "A lab test actor" should {
    val labTestActor = system.actorOf(Props[LabTestActor])
    "turn a string into uppercase" in {
      val message = "I am learning Akka!"
      labTestActor ! message

      // Receive one message from the test actor and assert that it conforms to the given type (after erasure).
      // Wait time is bounded by the given duration, with an AssertionFailure being thrown in case of timeout.
      // Returns the received object
      val reply = expectMsgType[String]
      assert(reply == "I AM LEARNING AKKA!")
    }

    "reply to a greeting" in {
      labTestActor ! "greeting"
      expectMsgAnyOf("hi", "hello")
    }

    "reply with favorite tech" in {
      labTestActor ! "favoriteTech"
      expectMsgAllOf("Scala", "Akka")
    }

    "reply with cool tech in a different way" in {
      labTestActor ! "favoriteTech"
      val messages = receiveN(2) // Seq[AnyRef], which are the received messages
      // free to do more complicated assertions
    }

    "reply with cool tech in a a fancy way" in {
      labTestActor ! "favoriteTech"

      // Receive one message from the test actor and assert that the given partial function accepts it.
      // Wait time is bounded by the given duration, with an AssertionFailure being thrown in case of timeout.
      // Use this variant to implement more complicated or conditional processing.
      // Returns the received object as transformed by the partial function
      expectMsgPF() {
        case "Scala" => // only care that PF is defined
        case "Akka" =>
      }
    }
  }

}

object BasicSpec {
  // An echo actor
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }

  class BlackHole extends Actor {
    override def receive: Receive = {
      Actor.emptyBehavior // equivalent to case _ =>
    }
  }

  class LabTestActor extends Actor {
    val random = new Random()

    override def receive: Receive = {
      case "greeting" => if (random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case "favoriteTech" =>
        sender() ! "Scala"
        sender() ! "Akka"
      case message: String => sender() ! message.toUpperCase()
    }
  }
}
