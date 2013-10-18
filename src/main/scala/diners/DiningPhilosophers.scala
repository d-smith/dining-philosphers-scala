package diners

import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._

sealed trait DinerMessage
case object Think extends DinerMessage
case object ProbeCurrentBehavior extends DinerMessage
case object Eat extends DinerMessage

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
