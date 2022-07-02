package part5infra

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}
import akka.dispatch.{ControlMessage, PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.{Config, ConfigFactory}

object Mailboxes extends App {

  // Mailboxes control how messages are stored for actors. They are data structures in actor reference that store messages

  val system = ActorSystem("MailboxDemo", ConfigFactory.load().getConfig("mailboxesDemo"))

  class SimpleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  // Mailboxes are regular queues

  /**
   * Interesting case #1 - Custom priority mailbox
   * P0 -> most important
   * P1
   * P2
   * P3
   */

  // step 1 - mailbox definition
  class SupportTicketPriorityMailbox(settings: ActorSystem.Settings, config: Config) // class definition should take these exact arguments
    extends UnboundedPriorityMailbox(PriorityGenerator {
      case message: String if message.startsWith("[P0]") => 0
      case message: String if message.startsWith("[P1]") => 1
      case message: String if message.startsWith("[P2]") => 2
      case message: String if message.startsWith("[P3]") => 3
      case _ => 4
    })

  // step 2 - make it known in the config
  // step 3 - attach the dispatcher to an actor
  val supportTicketLogger = system.actorOf(Props[SimpleActor].withDispatcher("support-ticket-dispatcher"))
  //  supportTicketLogger ! PoisonPill // even this will be postponed
  //  // Thread.sleep(1000) // with this, the actor dies in the meantime
  //  supportTicketLogger ! "[P3] this thing would be nice to have!"
  //  supportTicketLogger ! "[P0] this thing must be solved NOW!"
  //  supportTicketLogger ! "[P1] do this when you get a chance!"
  // after which time can I send another message and be prioritized accordingly? We don't know and we can't configure it

  /**
   * Interesting case #2 - Control-aware mailbox (some messages must be handled first regardless of what is in the queue)
   * we'll use UnboundedControlAwareMailbox
   */
  // step 1 - mark important messages as control messages
  case object ManagementTicket extends ControlMessage // Messages that extend ControlMessage trait will be handled with priority by control aware mailboxes.

  // step 2 - configure who get the mailbox: make the actor attach to the mailbox
  // method # 1
  val controlAwareActor = system.actorOf(Props[SimpleActor].withMailbox("control-mailbox"))
  //  controlAwareActor ! "[P0] this thing must be solved NOW!"
  //  controlAwareActor ! "[P1] do this when you get a chance!"
  //  controlAwareActor ! ManagementTicket

  // method # 2 - using deployment config
  val altControlAwareActor = system.actorOf(Props[SimpleActor], "altControlAwareActor")
  altControlAwareActor ! "[P0] this thing must be solved NOW!"
  altControlAwareActor ! "[P1] do this when you get a chance!"
  altControlAwareActor ! ManagementTicket
}
