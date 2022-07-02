package part3testing

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class TestProbeSpec extends TestKit(ActorSystem("TestProbeSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import TestProbeSpec._

  "A master actor" should {
    "register a slave" in {
      val master = system.actorOf(Props[Master])
      // TestProbes are special actors with assertion capabilities
      val slave = TestProbe("slave") // creating a fictitious slave actor

      // now we can test the interaction between master and slave test probe
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)
    }

    "send the work to the slave actor" in {
      // creating master actor inside each test, because it is stateful in this case (keeps total word count)
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workloadString = "I love Akka"
      master ! Work(workloadString)

      // testing the interaction between master actor and slave actor
      slave.expectMsg(SlaveWork(workloadString, testActor)) // testActor is the sender of the Work
      slave.reply(WorkCompleted(3, testActor)) // TestProbes can be instructed to send messages

      expectMsg(Report(3)) // testActor expects/receives Report(3)
    }

    "aggregate data correctly" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workloadString = "I love Akka"
      master ! Work(workloadString)
      master ! Work(workloadString)

      // in the meantime I don't have a slave actor (slave is passive, it is not sending any message back to master)
      slave.receiveWhile() {
        case SlaveWork(`workloadString`, `testActor`) => slave.reply(WorkCompleted(3, testActor)) // `` indicates it must match the exact value
      }

      expectMsg(Report(3))
      expectMsg(Report(6))
    }
  }

}

object TestProbeSpec {
  // Scenario
  /*
   Word counting actor hierarchy master-slave

   Send some work to the master
     - Master sends the slave a piece of work
     - Slave processes the work and replies to master
     - Master aggregates the result
   Master sends the total word count to the original requester
   */

  case class Work(text: String)

  case class SlaveWork(text: String, originalRequester: ActorRef)

  case class WorkCompleted(count: Int, originalRequester: ActorRef)

  case class Report(totalCount: Int)

  case class Register(slaveRef: ActorRef)

  case object RegistrationAck

  class Master extends Actor {
    override def receive: Receive = {
      case Register(slaveRef) =>
        sender() ! RegistrationAck
        context.become(online(slaveRef, 0))
      case _ => // ignore
    }

    def online(slaveRef: ActorRef, totalWordCount: Int): Receive = {
      case Work(text) => slaveRef ! SlaveWork(text, sender())
      case WorkCompleted(count, originalRequester) =>
        val newTotalWordCount = totalWordCount + count
        originalRequester ! Report(newTotalWordCount)
        context.become(online(slaveRef, newTotalWordCount))
    }
  }

  // class Slave extends Actor ..., which we don't care about in this test suite, here we are testing the master actor
}


