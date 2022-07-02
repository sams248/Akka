package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorSystem, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy.Stop
import akka.pattern.{BackoffOpts, BackoffSupervisor}

import java.io.File
import scala.io.Source
import scala.concurrent.duration._

object BackoffSupervisorPattern extends App {

  case object ReadFile

  // A simple actor that tries to read from an external file
  class FileBasedPersistentActor extends Actor with ActorLogging {

    var dataSource: Source = null

    override def preStart(): Unit = log.info("Persistent actor starting...")

    override def postStop(): Unit = log.warning("Persistent actor has stopped...")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit = log.warning("Persistent actor restarting...")

    override def receive: Receive = {
      case ReadFile =>
        if (dataSource == null) {
          // creates Source from file, using default character encoding, setting its description to filename.
          dataSource = Source.fromFile(new File("src/main/resources/testfiles/important_data.txt"))
          // dataSource = Source.fromFile(new File("src/main/resources/testfiles/important.txt"))
          log.info("I have just read some important data: " + dataSource.getLines().toList)
        }
    }
  }

  val system = ActorSystem("BackoffSupervisorDemo")
  // val simpleActor = system.actorOf(Props[FileBasedPersistentActor], "simpleActor")
  // simpleActor ! ReadFile

  val simpleSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onFailure(
      Props[FileBasedPersistentActor],
      "simpleBackoffActor",
      3.seconds, // then 6s, 12s, 24s
      30.seconds,
      0.2 // adds randomness to the time so all actors don't start at the same time
    )
  )

  // val simpleBackoffSupervisor = system.actorOf(simpleSupervisorProps, "simpleSupervisor")
  // simpleBackoffSupervisor ! ReadFile

  /*
   simpleSupervisor is created
    - simpleSupervisor creates a child called simpleBackoffActor (props of type FileBasedPersistentActor)
    - simpleSupervisor forwards any message to its child
    - supervision strategy is the default one (restarting on everything)
      - (when the child actor fails) first attempt restart after 3 seconds
      - (when the child actor fails again) next attempt is 2x the previous attempt
   */

  val stopSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onStop(
      Props[FileBasedPersistentActor],
      "stopBackoffActor",
      3.seconds, // then 6s, 12s, 24s
      30.seconds,
      0.2
    ).withSupervisorStrategy(
      OneForOneStrategy() {
        case _ => Stop
      }
    )
  )

  //  val stopSupervisor = system.actorOf(stopSupervisorProps, "stopSupervisor")
  //  stopSupervisor ! ReadFile

  class EagerFileBasedPersistentActor extends FileBasedPersistentActor {
    override def preStart(): Unit = {
      log.info("Eager actor starting")
      dataSource = Source.fromFile(new File("src/main/resources/testfiles/important_data.txt"))
    }
  }

  val eagerActor = system.actorOf(Props[EagerFileBasedPersistentActor])
  // ActorInitializationException => STOP (default behavior for this exception)

  val repeatedSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onStop(
      Props[EagerFileBasedPersistentActor],
      "stopBackoffActor",
      1.seconds,
      30.seconds,
      0.1
    )
  )

  val repeatedSupervisor = system.actorOf(repeatedSupervisorProps, "eagerSupervisor")

  /*
   eagerSupervisor
    - child eagerActor
      - will die on start with ActorInitializationException
      - trigger the supervision strategy in eagerSupervisor => STOP eagerActor
    - backoff will kick in after 1 second, 2s, 4s, 8s, 16s
   */
}
