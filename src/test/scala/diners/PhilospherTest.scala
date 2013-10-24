package diners

import org.scalatest.{WordSpec, FunSuite}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestFSMRef}
import akka.actor._
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration._
import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import akka.util.Timeout
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import diners.TestHelper._
import scala.util.Success
import scala.util.Failure


class PhilospherTest extends TestKit(ActorSystem("test-system"))
  with  ImplicitSender
  with WordSpec
  with MustMatchers {

  //val left = system.actorOf(Props[Fork])
  //val right = system.actorOf(Props[Fork])
  //val philosopher =  TestActorRef(Props(new Philosopher(left,right,"tester")))

  implicit val timeout = Timeout(5 seconds)

  "a philosopher" must {
    val philosopher = TestActorRef(Props(new Philosopher(self, self, "tester")))

    "start thinking when told to think" in {

      philosopher ! Think
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "thinking")

    }

    "become hungry and ask for forks" in {
      expectMsg( 5 seconds, Pickup(philosopher))
      expectMsg( 5 seconds, Pickup(philosopher))
    }

    "eat and eventually put down forks when granted requested forks" in {
      philosopher ! GotIt(self)
      philosopher ! GotIt(self)
      expectMsg(5 seconds, Putdown(philosopher))
      expectMsg(5 seconds, Putdown(philosopher))
    }

    "take a poison pill to not interfere with next tests" in {
      philosopher ! PoisonPill
    }
  }

  "another philospher" must {

    val philosopher = TestActorRef(Props(new Philosopher(self, self, "tester2")))

    "start thinking when told to think" in {

      philosopher ! Think
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "thinking")

    }

    "become hungry and ask for forks" in {
      expectMsg( 5 seconds, Pickup(philosopher))
      expectMsg( 5 seconds, Pickup(philosopher))
    }

    "give up when a fork is denied" in {
      philosopher ! InUse(self)
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "giving up")
    }

    "put down the fork when giving up then become thinking" in {
      philosopher ! GotIt(self)
      expectMsg(5 second, Putdown(philosopher))
      val state = philosopher ? ProbeCurrentBehavior
      assertState(state, "thinking")
    }

    "take a poison pill to not interfere with next tests" in {
      philosopher ! PoisonPill
    }

  }

  "and yet another philospher" must {

    val philosopher = TestActorRef(Props(new Philosopher(self, self, "tester3")))

    "start thinking when told to think" in {

      philosopher ! Think
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "thinking")

    }

    "become hungry and ask for forks" in {
      expectMsg( 5 seconds, Pickup(philosopher))
      expectMsg( 5 seconds, Pickup(philosopher))
    }

    "wait for second for when granted first fork" in {
      philosopher ! GotIt(self)
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "waitingForFork2")
    }

    "give up when denied the second fork and put down the first fork" in {
      philosopher ! InUse(self)
      expectMsg(1 second, Putdown(philosopher))
      val state = philosopher ? ProbeCurrentBehavior
      assertState(state, "thinking")
    }

    "take a poison pill to not interfere with next tests" in {
      philosopher ! PoisonPill
    }

  }

  "and the unluckiest philospher" must {

    val philosopher = TestActorRef(Props(new Philosopher(self, self, "tester4")))

    "start thinking when told to think" in {

      philosopher ! Think
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "thinking")

    }

    "become hungry and ask for forks" in {
      expectMsg( 5 seconds, Pickup(philosopher))
      expectMsg( 5 seconds, Pickup(philosopher))
    }

    "give up when denied the first fork" in {
      philosopher ! InUse(self)
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "giving up")
    }

    "start thinking when denied the second fork" in {
      philosopher ! InUse(self)
      val state = philosopher ? ProbeCurrentBehavior
      assertState(state, "thinking")
    }

    "take a poison pill to not interfere with next tests" in {
      philosopher ! PoisonPill
    }

  }


}
