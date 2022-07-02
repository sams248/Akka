package part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object ThreadModelLimitations extends App {

  // 1: OOP Encapsulation is only valid in the single-threaded model

  /*
    In object-oriented computer programming (OOP) languages, the notion of encapsulation (or OOP Encapsulation) refers
    to the bundling of data, along with the methods that operate on that data, into a single unit.
    Many programming languages use encapsulation frequently in the form of classes.
   */

  /*
    We might think that an instance of this class in encapsulated, because only the "outside" can call methods on the object
    and the object is solely responsible for it's internal state, but in multi-threaded environment this principle breaks down!
   */
  class BankAccount(private var amount: Int) {
    override def toString = s"$amount"

    def withdraw(money: Int): Unit = synchronized {
      this.amount -= money
    }

    def deposit(money: Int): Unit = synchronized {
      this.amount += money
    }

    def getAmount: Int = amount
  }

  val account = new BankAccount(2000)

  val depositThreads: Seq[Thread] = (1 to 1000).map(_ => new Thread(() => account.deposit(1)))
  val withdrawThreads: Seq[Thread] = (1 to 1000).map(_ => new Thread(() => account.withdraw(1)))

  def demoRace(): Unit = {
    (depositThreads ++ withdrawThreads).foreach(_.start())
    Thread.sleep(1000)
    println(account.getAmount) // without synchronized, we may not get 2000, because OOP encapsulation is broken
  }

  // demoRace()

  /*
    - We don't know when the threads are finished
    - Race conditions

    Solution: synchronization: locks to the rescue, but other problems:
      - dead-locks
      - live-locks
   */

  // We need a data structure that is fully encapsulated in multi-threaded and distributed environments and does not require locks!

  // 2 - Delegating a task to background (send a signal to a running thread) is a pain

  // e.g. you have a running thread and you want to pass a runnable to that thread.
  var task: Runnable = null

  val runningThread: Thread = new Thread(() => {
    while (true) { // a thread that will run forever
      while (task == null) {
        runningThread.synchronized {
          println("[background] waiting for a task...")
          runningThread.wait()
        }
      }

      task.synchronized {
        println("[background] I have a task!")
        task.run()
        task = null
      }
    }
  })

  def delegateToBackgroundThread(r: Runnable): Unit = {
    if (task == null) {
      task = r
      runningThread.synchronized {
        runningThread.notify()
      }
    }
  }

  def demoBackgroundDelegation(): Unit = {
    runningThread.start()
    Thread.sleep(1000)
    delegateToBackgroundThread(() => println("I'm running from another thread"))
    Thread.sleep(1000)
    delegateToBackgroundThread(() => println("This should run in the background again"))
  }

  // demoBackgroundDelegation()

  /*
   Other problems:
    - Other signals?
    - Multiple background tasks and threads?
    - Who gave the signal?
    - What if I crash?
   */

  /*
   We need a data structure that:
    - can safely receive messages
    - can identify the sender
    - is easily identifiable
    - can guard against errors
   */

  // 3. Tracing and dealing with errors is a pain in multi-threaded/distributed apps

  // Sum 1M numbers in between 10 threads
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val futures: Seq[Future[BigInt]] = (0 to 9)
    .map(i => BigInt(100000 * i) until BigInt(100000 * (i + 1))) // 0 - 99999, 100000 - 199999, and so on
    .map(range => Future {
      // bug
      if (range.contains(BigInt(546732))) throw new RuntimeException("invalid number")
      range.sum
    })

  val sumFuture: Future[BigInt] = Future.reduceLeft(futures)(_ + _)
  sumFuture.onComplete(println) // Failure(java.lang.RuntimeException: invalid number) debugging this is a pain
}