package diners

import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.actor.ActorSystem
import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import akka.pattern.ask
import scala.util.{Failure, Success}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future


class ForkTest extends TestKit(ActorSystem("test-system"))
  with ImplicitSender
  with WordSpec
  with MustMatchers {

    def assertState(state:Future[Any], expectedValue: String) : Unit = {
      state.value.get match {
        case Success(s) => { s must be === expectedValue  }
        case Failure(e) => throw e
      }
    }

    val fork = TestActorRef[Fork]
    implicit val timeout = Timeout(5 seconds)

    "a fork" must {
      "be available initially" in {
        val state = fork ? ProbeCurrentBehavior
        assertState(state, "available")
      }

      "be in use after being picked up" in {
        fork ! Pickup(self)
        expectMsg(GotIt(fork))

        val state = fork ? ProbeCurrentBehavior
        assertState(state, "inUse")

      }

      /*"not given to another diner when in use" in {

      }

      "become available when put down" in {

      }

      "can be taken after being put down" in {
        assert(true)
      }
      */
    }
}
