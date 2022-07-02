package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.util.Random

class TimedAssertionsSpec extends TestKit(ActorSystem("TimedAssertionsSpec", ConfigFactory.load().getConfig("specialTimedAssertionsConfig")))
  with ImplicitSender with AnyWordSpecLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import TimedAssertionsSpec._

  "A worker actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])

    "reply with the expected result in a timely manner" in {
      within(500.millis, 1.second) {
        workerActor ! "work"
        expectMsg(WorkResult(44))
      }
    }

    "reply with valid work at a reasonable cadence" in {
      within(1.second) {
        workerActor ! "workSequence"

        // Receive a series of messages until one does not match the given partial function or the idle timeout is met
        // (disabled by default) or the overall maximum duration is elapsed or expected messages count is reached.
        // Returns the sequence of messages.
        val results: Seq[Int] = receiveWhile[Int](max = 2.seconds, idle = 500.millis, messages = 10) {
          case WorkResult(result) => result
        }
        assert(results.sum > 5)
      }
    }

    "reply to a test probe in a timely manner" in {
      within(1.seconds) {
        val probe = TestProbe()
        probe.send(workerActor, "work")
        probe.expectMsg(WorkResult(44)) // configured with timeout of 0.3 seconds => test will fail
      }
    }
  }

}

object TimedAssertionsSpec {

  case class WorkResult(result: Int)

  // Testing scenario
  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work" =>
        // long/hard computation
        Thread.sleep(500)
        sender() ! WorkResult(44)
      case "workSequence" =>
        val r = new Random()
        for (_ <- 1 to 10) {
          Thread.sleep(r.nextInt(50))
          sender() ! WorkResult(1)
        }
    }
  }
}

