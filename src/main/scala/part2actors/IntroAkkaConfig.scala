package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfig extends App {

  // Configuration stores the values that can tweak the behavior of Akka

  /**
   * 1 - Inline configuration
   */
  // configuration is a piece of text, so we can write:
  val configString =
  """
    | akka {
    |   loglevel = "DEBUG"
    | }
  """.stripMargin
  // now we can turn the string into configuration object:
  val config = ConfigFactory.parseString(configString) // returns the parsed configuration
  val system = ActorSystem("ConfigurationDemo", ConfigFactory.load(config))
  // at this point config is loaded into the actor system

  class SimpleLoggingActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val actor = system.actorOf(Props[SimpleLoggingActor])
  actor ! "A message to remember!"

  /**
   * 2 - Configuration file (most common)
   * source/main/resources/application.conf
   */

  val defaultConfigFileSystem = ActorSystem("DefaultConfigFileDemo")
  val defaultConfigActor = defaultConfigFileSystem.actorOf(Props[SimpleLoggingActor])
  defaultConfigActor ! "Remember me!" // akka will look at source/main/resources/application.conf by default

  /**
   * 3 - Separate configurations in the same file
   */
  val specialConfig = ConfigFactory.load().getConfig("mySpecialConfig")
  val specialConfigFileSystem = ActorSystem("SpecialConfigFileDemo", specialConfig)
  val specialConfigActor = specialConfigFileSystem.actorOf(Props[SimpleLoggingActor])
  specialConfigActor ! "Remember me, I am special!"

  /**
   * 4 - Separate configurations in another file
   */
  val separateConfig = ConfigFactory.load("secretFolder/secretConfiguration.conf")
  println(s"separate config loglevel: ${separateConfig.getString("akka.loglevel")}")

  /**
   * 5 - Different file formats
   * e.g. JSON or properties
   */
  val jsonConfig = ConfigFactory.load("json/jsonConf.json")
  println(s"json config: ${jsonConfig.getString("aJsonProperty")}")
  println(s"json config: ${jsonConfig.getString("akka.loglevel")}")

  val propsConfig = ConfigFactory.load("props/propsConf.properties")
  println(s"properties config: ${propsConfig.getString("my.simpleProperty")}")
  println(s"properties config: ${propsConfig.getString("akka.loglevel")}")

}
