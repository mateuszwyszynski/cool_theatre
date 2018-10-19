package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import theatre.Neurone._
import theatre.Output.EnterTheMatrix
import theatre.Reality.Action
import theatre.VectorTools._

object Output {
  def props(position: Point, inputRadius: Double): Props = Props(new Output(position, inputRadius))

  final case class EnterTheMatrix(reality: ActorRef)
}

class Output(position: Point, inputRadius: Double) extends Actor with ActorLogging {
  def receive: Receive = {
    case EnterTheMatrix(reality) =>
      context.become(insideTheMatrix(reality))
  }

  def insideTheMatrix(reality: ActorRef): Receive = {
    case LookingForConnections(inpPos, axCoor) =>
      if(segmentIntersectsBall(inpPos, add(inpPos, axCoor), position, inputRadius)) {
        sender ! EstablishConnection()
      } else {}
    case Signal(value) =>
      val magicNumber = 0.5
      if(value >= magicNumber) reality ! Action(1) else reality ! Action(0)
  }
}
