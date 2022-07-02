package part5infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

// Dispatchers control how messages are being sent and handled
object Dispatchers extends App {

  class Counter extends Actor with ActorLogging {
    var count = 0

    override def receive: Receive = {
      case message =>
        count += 1
        log.info(s"[$count] $message")
    }
  }

  val system = ActorSystem("DispatchersDemo") // ConfigFactory.load().getConfig("dispatchersDemo")

  // Method 1. programmatic / in code
  val actors = for (i <- 1 to 10) yield system.actorOf(Props[Counter].withDispatcher("my-dispatcher"), s"counter_$i")

  val r = new Random()
  //  for (i <- 1 to 1000) {
  //    actors(r.nextInt(10)) ! i
  //  }

  // Method 2. from config
  val samActor = system.actorOf(Props[Counter], "sam") // config will take care of attaching specified dispatcher to the actor

  /*
   * Dispatchers implement the ExecutionContext trait
   */

  class DBActor extends Actor with ActorLogging {
    // implicit val executionContext: ExecutionContext = context.dispatcher
    // solution 1
    implicit val executionContext: ExecutionContext = context.system.dispatchers.lookup("my-dispatcher")
    // solution 2 - use Router

    override def receive: Receive = {
      case message => Future {
        // wait on a response
        Thread.sleep(5000)
        log.info(s"Success: $message")
      }
    }
  }

  val dbActor = system.actorOf(Props[DBActor])
  // dbActor ! "the meaning of life is 42"

  val nonBlockingActor = system.actorOf(Props[Counter])
  for (i <- 1 to 1000) {
    val message = s"Important message $i"
    dbActor ! message
    nonBlockingActor ! message
  }
}
