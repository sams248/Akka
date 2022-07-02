package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}

object ActorLoggingDemo extends App {

  class SimpleActorWithExplicitLogger extends Actor {
    // 1 - Explicit logging
    val logger: LoggingAdapter = Logging(context.system, this)

    /*
     Logging is done in 4 different levels:
      1. DEBUG
      2. INFO
      3. WARN/WARNING
      4. ERROR
     */

    override def receive: Receive = {
      case message => logger.info(message.toString) // Log it
    }
  }

  val system = ActorSystem("LoggingDemo")
  val actor = system.actorOf(Props[SimpleActorWithExplicitLogger])

  actor ! "Logging a simple message"

  // 2 - ActorLogging
  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b) => log.info("Two things: {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }

  val simplerActor = system.actorOf(Props[ActorWithLogging])
  simplerActor ! "Logging a simple message by extending a trait"

  simplerActor ! (44, 88)

  // Note: Logging is done asynchronously to minimize performance impact
  // Akka logging is done with actors!
  // You can change the logger, e.g. SLF4J

}
