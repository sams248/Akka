package part1recap

import scala.concurrent.Future
import scala.util.{Failure, Success}

object MultiThreadingRecap extends App {

  // Creating a new Thread:
  //  val aThread = new Thread(new Runnable {
  //    override def run(): Unit = println("I am running in parallel!")
  //  })

  // Creating a new Thread with syntax sugar:
  val aThread = new Thread(() => println("I am running in parallel!"))
  // To start a Thread:
  aThread.start()
  // To finish a Thread:
  aThread.join()

  // The problem with threads is that they are unpredictable, different runs produce different results
  val helloThread = new Thread(() => (1 to 100).foreach(_ => println("Hello")))
  val goodbyeThread = new Thread(() => (1 to 100).foreach(_ => println("Goodbye")))
  // We will see a mix of Hellos and Goodbyes printed (probably in different orders in different runs)
  helloThread.start()
  goodbyeThread.start()

  class BankAccount(private var amount: Int) {
    override def toString: String = "" + amount

    def withdraw(money: Int): Unit = this.amount -= money // This method is not thread safe (atomic)

    // This is thread-safe, because multiple threads cannot evaluate the expression at the same time (one will have to block until the other is done)
    def safeWithdraw(money: Int): Unit = this.synchronized {
      this.amount -= money
    }
  }

  // Inter-thread communication on the JVM can be done using wait-notify mechanism

  // Scala Futures

  import scala.concurrent.ExecutionContext.Implicits.global

  val aFuture = Future {
    // long computation - on a different thread
    42
  }

  // callbacks
  aFuture.onComplete {
    case Success(42) => println("I found the meaning of life!")
    case Failure(exception) => println("Something happened with the meaning of life!")
  }

  val aProcessedFuture = aFuture.map(_ + 1) // Future with 42
  val aFlatFuture = aFuture.flatMap {
    value => Future(value + 2)
  } // Future with 44
  val filteredFuture = aFuture.filter(_ % 2 == 0) // A future with the filter applied or NoSuchElementException
  // for-comprehension
  val aNonsenseFuture = for {
    meaningOfLife <- aFuture
    filteredMeaning <- filteredFuture
  } yield meaningOfLife + filteredMeaning

  // andThen, recover/ recoverWith

  // Promises
}
