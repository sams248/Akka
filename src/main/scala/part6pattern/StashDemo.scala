package part6pattern

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

/*
  Stashes allow actors to set messages aside for later that they can not or should not process at this exact moment
  When the time is right (e.g. when actor changes behavior using context.become),
  is a good time to prepend them to the mailbox and process them again
 */

object StashDemo extends App {

  /*
   Resource actor
    - Open => it can receive read/write requests to the resource
    - otherwise it will postpone all read/write requests until the state is open

    ResourceActor is initially closed
      - Open => switch to the open state
      - Read, Write messages are POSTPONED

    ResourceActor is open
      - Read, Write are handled
      - Close => switch to the closed state

    [Open, Read, Read, Write]
      - switch to the open state
      - read the data
      - read the data again
      - write the data

    [Read, Open, Write]
      - stash Read
        Stash: [Read]
      - Open => switch to the open state
        Mailbox: [Read, Write] (Read is prepended)
      - read and write are handled
   */

  case object Open

  case object Close

  case object Read

  case class Write(data: String)

  // Step 1: mix-in the Stash trait
  class ResourceActor extends Actor with ActorLogging with Stash {
    private var innerData: String = ""

    override def receive: Receive = closed

    def closed: Receive = {
      case Open =>
        log.info("Opening resource...")
        // Step 3: unstashAll shen you switch the message handle
        unstashAll() // Prepends all messages in the stash to the mailbox, and then clears the stash.
        context.become(open)
      case message =>
        log.info(s"Stashing $message because I can't handle it in the closed state")
        // Step 2: stash away what you can't handle
        stash() // Adds the current message (the message that the actor received last) to the actor's stash.
    }

    def open: Receive = {
      case Read =>
        // do some actual computation
        log.info(s"I have read $innerData")
      case Write(data) =>
        log.info(s"I am writing $data")
        innerData = data
      case Close =>
        log.info("Closing resource")
        unstashAll()
        context.become(closed)
      case message =>
        log.info(s"Stashing $message because I can't handle it in the open state")
        stash()
    }
  }

  val system = ActorSystem("StashDemo")
  val resourceActor = system.actorOf(Props[ResourceActor])

  resourceActor ! Read // stashed
  resourceActor ! Open // switch to open; I have read ""
  resourceActor ! Open // stashed
  resourceActor ! Write("I love stash") // I am writing I love stash
  resourceActor ! Close // switch to closed; switch to open
  resourceActor ! Read // I have read I love stash
}
