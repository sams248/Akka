package part5infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Terminated}
import akka.routing.{ActorRefRoutee, Broadcast, FromConfig, RoundRobinGroup, RoundRobinPool, RoundRobinRoutingLogic, Router}
import com.typesafe.config.ConfigFactory

object Routers extends App {
  /*
   Routers are extremely useful when we want to delegate or spread work between multiple actors of the same kind.
   They are middle level actors that forward messages to other actors, either created by routers or from the outside.
   */

  /**
   * Method 1: Manual Router
   */
  class Master extends Actor {
    // Step 1 : Create 5 actor routees based off Slave actors
    private val slaves = for (s <- 1 to 5) yield {
      val slave = context.actorOf(Props[Slave], s"slave_$s")
      // Registers this actor as a Monitor for the provided ActorRef. This actor will receive a Terminated(subject) message when watched actor is terminated.
      context.watch(slave)
      ActorRefRoutee(slave)
    }

    // Step 1 : Define router
    private var router = Router(RoundRobinRoutingLogic(), slaves)

    override def receive: Receive = {
      // Step 4: Handle the termination/lifecycle of the routees
      case Terminated(ref) =>
        router = router.removeRoutee(ref)
        val newSlave = context.actorOf(Props[Slave])
        context.watch(newSlave)
        router = router.addRoutee(newSlave)
      // Step 3: Route the messages
      case message =>
        router.route(message, sender())
    }
  }

  class Slave extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("RoutersDemo", ConfigFactory.load().getConfig("routersDemo"))
  val master = system.actorOf(Props[Master])
  //  for (i <- 1 to 10) {
  //    master ! s"[$i] Hello from the world"
  //  }

  /**
   * Method 2: A router actor with its own children
   * POOL router
   */
  // Method 2.1: programmatically (in code)
  val poolMaster = system.actorOf(RoundRobinPool(5).props(Props[Slave]), "simplePoolMaster")
  //    for (i <- 1 to 10) {
  //      poolMaster ! s"[$i] Hello from the world"
  //    }

  // Method 2.1: from configurations
  val poolMaster2 = system.actorOf(FromConfig.props(Props[Slave]), "poolMaster2")
  //  for (i <- 1 to 10) {
  //    poolMaster2 ! s"[$i] Hello from the world"
  //  }

  /**
   * Method 3: Router with actors created elsewhere
   * Group router
   */
  // .. in another part of my application
  val slaveList = (1 to 5).map(i => system.actorOf(Props[Slave], s"slave_$i")).toList

  // need their paths
  val slavePaths = slaveList.map(slaveRef => slaveRef.path.toString)

  // 3.1 in the code
  val groupMaster = system.actorOf(RoundRobinGroup(slavePaths).props())
  //  for (i <- 1 to 10) {
  //    poolMaster2 ! s"[$i] Hello from the world"
  //  }

  // 3.2 from configuration
  val groupMaster2 = system.actorOf(FromConfig.props(), "groupMaster2")
  //  for (i <- 1 to 10) {
  //    poolMaster2 ! s"[$i] Hello from the world"
  //  }

  /**
   * Handling special messages
   */
  groupMaster2 ! Broadcast("Hello, everyone!")

  // PoisonPill and Kill are NOT routed, they are handled by the routing actor
  // AddRoutee, RemoveRoutee, GetRoutee are handled only by the routing actor
}
