package diners

import org.scalatest.{WordSpec, FunSuite}
import akka.testkit.{TestActorRef, TestKit, TestFSMRef}
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
  with WordSpec
  with MustMatchers {

  val left = system.actorOf(Props[Fork])
  val right = system.actorOf(Props[Fork])
  val philosopher =  TestActorRef(Props(new Philosopher(left,right,"tester")))
  implicit val timeout = Timeout(5 seconds)

  "a philosopher" must {
    "start thinking when told to think" in {

      philosopher ! Think
      val future = philosopher ? ProbeCurrentBehavior
      assertState(future, "thinking")
    }
  }




}
