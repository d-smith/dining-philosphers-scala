package diners

import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._

sealed trait DinerMessage
case object Think extends DinerMessage
case object ProbeCurrentBehavior extends DinerMessage
case object Eat extends DinerMessage
case class Pickup(diner: ActorRef)
case class Putdown(diner: ActorRef)
case class InUse(fork: ActorRef)
case class GotIt(fork:ActorRef)

class Fork extends Actor {
  import context._

  def inUse(diner: ActorRef): Receive = {
    case Putdown(`diner`) => {
      become(available)
    }
    case Pickup(diner) => sender ! InUse(self)
    case ProbeCurrentBehavior => sender ! "inUse"

  }

  def available: Receive = {
    case Pickup(diner) => {
      become(inUse(diner))
      diner ! GotIt(self)
    }

    case ProbeCurrentBehavior => sender ! "available"

  }

  def receive = available
}

class Philosopher extends Actor {
  import context._

  def hungry : Receive = {
    case ProbeCurrentBehavior => sender ! "hungry"
  }

  def thinking : Receive = {
    case Eat => become(hungry)
    case ProbeCurrentBehavior => sender ! "thinking"
  }

  def receive = {
    case Think => {
      become(thinking)
      system.scheduler.scheduleOnce(2 seconds, self, Eat)
    }
  }
}

object DiningPhilosophers {

}
