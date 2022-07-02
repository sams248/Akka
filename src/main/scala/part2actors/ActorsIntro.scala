package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  // Part 1 - Actor Systems

  // The actor system is a heavy weight data structure that controls a number of threads under the hood, which then allocates to running actors
  // It is recommended to have only one actor system for an application, unless there is a good reason to have more
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // Part 2 - Creating Actors

  // Actors are uniquely identified within an Actor Systems
  // Messages are passed and processed asynchronously (you send a msg when you need to, and the actor will respond when it can)
  // Each actor may respond differently
  // Actors are (really) encapsulated

  class WordCountActor extends Actor {
    // Internal data
    var totalWords = 0

    // Behavior
    def receive: Receive = { // Receive is an alias for PartialFunction[Any, Unit]
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // Part 3 - Instantiate the Actor

  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")
  // ActorRef is the data structure that Akka exposes to us as programmers, and we can only communicate with the actor via this reference

  // Part 4 - Communicate

  // asynchronous!
  wordCounter ! "I am learning Akka!" // infix notation of wordCounter.!("I am learning Akka!")
  // ! method is also known as "tell"
  anotherWordCounter ! "A different message"

  // Creating actors with constructive arguments

  // Best practice: define a companion object with props method
  object Person {
    def props(name: String): Props = Props(new Person(name))
  }

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
    }
  }

  // val person = actorSystem.actorOf(Props(new Person("Sam")))
  val person = actorSystem.actorOf(Person.props("Sam"))
  person ! "hi"
}
