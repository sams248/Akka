package part6pattern

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, OneInstancePerTest}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class FSMMotivationSpec extends TestKit(ActorSystem("FSMMotivationSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with OneInstancePerTest {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import FSMMotivationSpec._

  "a vending machine" should {
    passTheseTests(Props[VendingMachine])
  }

  "a finite state vending machine" should {
    passTheseTests(Props[VendingMachine])
  }

  def passTheseTests(props: Props): Unit = {
    "error when not initialized" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! RequestProduct("coke")
      expectMsg(VendingError(MACHINE_NOT_INITIALIZED))
    }

    "report a product not available" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 1))
      vendingMachine ! RequestProduct("sandwich")
      expectMsg(VendingError(PRODUCT_NOT_AVAILABLE))
    }

    "throw a timeout if I don't insert money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 1))
      vendingMachine ! RequestProduct("coke")

      expectMsg(Instruction("Please insert 1 dollars!"))
      within(1.5.seconds) {
        expectMsg(VendingError(REQUEST_TIMED_OUT))
      }
    }

    "handle the reception of partial money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars!"))

      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("Please insert 2 dollars!"))

      within(1.5.seconds) {
        expectMsg(VendingError(REQUEST_TIMED_OUT))
        expectMsg(GiveBackChange(1))
      }
    }

    "deliver the product if I insert all the money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars!"))

      vendingMachine ! ReceiveMoney(3)
      expectMsg(Deliver("coke"))
    }

    "give back change and be able to request money for a new product" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars!"))

      vendingMachine ! ReceiveMoney(4)
      expectMsg(Deliver("coke"))
      expectMsg(GiveBackChange(1))

      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3 dollars!"))
    }
  }

}

object FSMMotivationSpec {

  /*
   Vending Machine
   */

  case class Initialize(inventory: Map[String, Int], prices: Map[String, Int])

  case class RequestProduct(product: String)

  case class Instruction(instruction: String) // The message the VM will show on its screen

  case class ReceiveMoney(amount: Int)

  case class Deliver(product: String)

  case class GiveBackChange(amount: Int)

  case class VendingError(reason: String)

  case object ReceiveMoneyTimeout

  val MACHINE_NOT_INITIALIZED = "MachineNotInitialized"
  val PRODUCT_NOT_AVAILABLE = "ProductNotAvailable"
  val REQUEST_TIMED_OUT = "RequestTimedOut"
  val COMMAND_NOT_FOUND = "CommandNotFound"

  class VendingMachine extends Actor with ActorLogging {
    implicit val executionContext: ExecutionContext = context.dispatcher

    def receive: Receive = idle

    def idle: Receive = {
      case Initialize(inventory, prices) => context.become(operational(inventory, prices))
      case _ => sender() ! VendingError(MACHINE_NOT_INITIALIZED)
    }

    def operational(inventory: Map[String, Int], prices: Map[String, Int]): Receive = {
      case RequestProduct(product) =>
        inventory.get(product) match {
          case None | Some(0) => sender() ! VendingError(PRODUCT_NOT_AVAILABLE)
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price dollars!")
            context.become(
              waitForMoney(
                inventory,
                prices,
                product,
                0,
                startReceiveMoneyTimeoutSchedule,
                sender()))
        }
    }

    def waitForMoney(
                      inventory: Map[String, Int],
                      prices: Map[String, Int],
                      product: String,
                      money: Int,
                      moneyTimeoutSchedule: Cancellable,
                      requester: ActorRef): Receive = {
      case ReceiveMoneyTimeout =>
        requester ! VendingError(REQUEST_TIMED_OUT)
        if (money > 0) requester ! GiveBackChange(money)
        context.become(operational(inventory, prices))
      case ReceiveMoney(amount) =>
        moneyTimeoutSchedule.cancel()
        val price = prices(product)
        if (money + amount >= price) {
          // user buys product
          requester ! Deliver(product)
          val changeIfAny = money + amount - price
          // deliver the change
          if (changeIfAny > 0) requester ! GiveBackChange(changeIfAny)
          // update inventory
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          context.become(operational(newInventory, prices))
        } else {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"Please insert $remainingMoney dollars!")
          context.become(
            waitForMoney(
              inventory, // doesn't change
              prices, // doesn't change
              product, // doesn't change
              money + amount, // user has inserted some money
              startReceiveMoneyTimeoutSchedule, // need to set te timeout again
              requester))
        }
    }

    def startReceiveMoneyTimeoutSchedule: Cancellable =
      context.system.scheduler.scheduleOnce(1.second) {
        self ! ReceiveMoneyTimeout
      }
  }
}