package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Kill, PoisonPill, Props, Terminated}
import part2actors.ChildActors
import part2actors.ChildActors.Child
import part4faulttolerance.StartingStoppingActors.Parent.StartChild

object StartingStoppingActors extends App {

  val system = ActorSystem("StoppingActorsDemo")

  object Parent {
    case class StartChild(name: String)

    case class StopChild(name: String)

    case object Stop
  }

  class Parent extends Actor with ActorLogging {

    import Parent._

    override def receive: Receive = withChildren(Map.empty)

    def withChildren(children: Map[String, ActorRef]): Receive = {
      case StartChild(name) =>
        log.info(s"Starting child $name")
        context.become(withChildren(children + (name -> context.actorOf(Props[Child], name))))
      case StopChild(name) =>
        log.info(s"Stopping child with the name $name")
        val maybeChild = children.get(name)
        maybeChild.foreach(childRef => context.stop(childRef)) // stop is non-blocking/asynchronous
      case Stop =>
        log.info("Stopping myself")
        context.stop(self) // stops children first
      case message =>
        log.info(message.toString)
    }
  }

  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
   * Method 1 - using context.stop
   */

  //  import Parent._
  //
  //  val parent = system.actorOf(Props[Parent], "parent")
  //  parent ! StartChild("child1")
  //  val child1 = system.actorSelection("/user/parent/child1")
  //  child1 ! "Hi kid!"
  //  parent ! StopChild("child1")
  //  // for (_ <- 1 to 20) child ! "are you still alive?"
  //  parent ! StartChild("child2")
  //  val child2 = system.actorSelection("/user/parent/child2")
  //  child2 ! "Hi 2nd kid!"
  //  parent ! Stop // this will also stop all the child actors
  //  for (_ <- 1 to 10) parent ! "Parent, are you still alive?"
  //  for (i <- 1 to 100) child2 ! s"[$i]Second kid, are you still alive?"

  /**
   * Method 2 - using special messages
   */
  //  val looseActor = system.actorOf(Props[Child])
  //  looseActor ! "Hello, loose actor!"
  //  looseActor ! PoisonPill // A message all Actors will understand, that when processed will terminate the Actor permanently.
  //  looseActor ! "loose actor, are you still there?!"
  //
  //  val abruptlyTerminateActor = system.actorOf(Props[Child])
  //  abruptlyTerminateActor ! "You are about to be terminated!!"
  //  abruptlyTerminateActor ! Kill // A message all Actors will understand, that when processed will make the Actor throw an ActorKilledException, which will trigger supervision.
  //  abruptlyTerminateActor ! "You have been terminated!!"

  /**
   * Method 2 - using special messages
   */
  class Watcher extends Actor with ActorLogging {

    import Parent._

    override def receive: Receive = {
      case StartChild(name) =>
        val child = context.actorOf(Props[Child], name)
        log.info(s"Started and watching child $name")
        context.watch(child)
      case Terminated(ref) =>
        log.info(s"The reference that I am watching $ref has been stopped!")
    }
  }

  val watcher = system.actorOf(Props[Watcher], "watcher")
  watcher ! StartChild("watchedChild")
  val watchedChild = system.actorSelection("/user/watcher/watchedChild")
  Thread.sleep(500)

  watchedChild ! PoisonPill

}
