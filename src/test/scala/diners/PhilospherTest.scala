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


class PhilospherTest extends TestKit(ActorSystem("test-system"))
  with WordSpec
  with MustMatchers {

  val philosopher =  TestActorRef[Philosopher]
  implicit val timeout = Timeout(5 seconds)

  "a philosopher" must {
    "start thinking when told to think" in {

      philosopher ! Think
      val future = philosopher ? ProbeCurrentBehavior
      future.value.get match {
        case Success(s) => { s must be === "thinking"  }
        case Failure(e) => throw e
      }
    }
  }
  
  "a thinking philosopher" must {
    "become hungry" in {
      //philosopher ! Eat
      Thread.sleep(3000)
      val future = philosopher ? ProbeCurrentBehavior
      future.value.get match {
        case Success(s) => { s must be === "hungry"  }
        case Failure(e) => throw e
      }
    }

  }
}
