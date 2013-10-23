package diners

import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Random

sealed trait DinerMessage
case object Think extends DinerMessage
case object ProbeCurrentBehavior extends DinerMessage
case object Eat extends DinerMessage
case class Pickup(diner: ActorRef)  extends DinerMessage
case class Putdown(diner: ActorRef)  extends DinerMessage
case class InUse(fork: ActorRef) extends DinerMessage
case class GotIt(fork:ActorRef) extends DinerMessage
case object StartEating extends DinerMessage

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

class Philosopher(leftFork: ActorRef, rightFork: ActorRef, name:String) extends Actor {
  import context._

  val random = new Random

  def getWait() : Int = {
    random.nextInt(3) + 1
  }

  def eating : Receive = {
    case Think => {
      val leftForkName = leftFork.path.name
      val rightForkName = rightFork.path.name
      println(s"$name nom nom nom with fork $leftForkName and $rightForkName")
      leftFork ! Putdown(self)
      rightFork ! Putdown(self)
      become(thinking)
      system.scheduler.scheduleOnce(getWait() seconds, self, Eat)
    }
  }

  def givingUp : Receive = {
    case GotIt(fork) => {
      println(s"$name got fork while giving up... put it down")
      fork ! Putdown(self)
      become(thinking)
      system.scheduler.scheduleOnce(getWait() seconds, self, Eat)
    }

    case InUse(_) => {
      println(s"$name denied second fork while giving up...")
      become(thinking)
      system.scheduler.scheduleOnce(getWait() seconds, self, Eat)
    }
  }

  def waitingForFork2(fork: ActorRef) : Receive = {
    case GotIt(`fork`) => {
      println(s"$name got second fork - starts eating")
      become(eating)
      system.scheduler.scheduleOnce(getWait() seconds, self, Think)
    }

    case InUse(`rightFork`) => {
      println(s"$name denied fork... back to thinking")
      leftFork ! Putdown(self)
      become(thinking)
      system.scheduler.scheduleOnce(getWait() seconds, self, Eat)
    }
    case InUse(`leftFork`) => {
      println(s"$name denied fork... back to thinking")
      rightFork ! Putdown(self)
      become(thinking)
      system.scheduler.scheduleOnce(getWait() seconds, self, Eat)
    }

  }

  def waitingForFork1 : Receive = {
    case GotIt(`leftFork`) => {
      println(s"$name got left fork")
      become(waitingForFork2(rightFork))
    }
    case GotIt(`rightFork`) => {
      println(s"$name got right fork")
      become(waitingForFork2(leftFork))
    }
    case InUse(_) => {
      println(s"$name denied a fork... giving up")
      become(givingUp)
    }
    case ProbeCurrentBehavior => {
      sender ! "waitingForFork1"
    }
  }

  def hungry : Receive = {
    case StartEating => {
      println(s"$name wants to start eating")
      become(waitingForFork1)
      leftFork ! Pickup(self)
      rightFork ! Pickup(self)
    }

    case ProbeCurrentBehavior => sender ! "hungry"
  }

  def thinking : Receive = {
    case Eat => {
      println(s"$name becomes hungry")
      become(hungry)
      self ! StartEating
    }
    case ProbeCurrentBehavior => sender ! "thinking"
  }

  def receive = {
    case Think => {
      println(s"$name starts thinking")
      become(thinking)
      system.scheduler.scheduleOnce(getWait() seconds, self, Eat)
    }
  }
}

object DiningPhilosophers {
  val system = ActorSystem()

  def main(args:Array[String]) : Unit = {
    val forks = for(i <- 1 to 5) yield system.actorOf(Props[Fork], s"Fork$i")

    val diners = for {
      (name,i) <- List("alice","bob","carol","david","emily").zipWithIndex
    } yield system.actorOf(Props(classOf[Philosopher],forks(i), forks((i+1)%5),name))

    diners.foreach(_ ! Think)
  }
}
