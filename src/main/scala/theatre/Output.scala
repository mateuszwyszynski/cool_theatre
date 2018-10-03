package theatre

import akka.actor._
import theatre.Neurone._
import theatre.VectorTools._

object Output {
  def props(position: Point, inputRadius: Double): Props = Props(new Output(position, inputRadius))
}

class Output(position: Point, inputRadius: Double) extends Actor with ActorLogging {
  def receive: Receive = {
    case LookingForConnections(inpPos, axCoor) =>
      if(segmentIntersectsBall(inpPos, add(inpPos, axCoor), position, inputRadius)) {
        log.info("New input neurone:" + sender + "Sending my coordinates.")
        sender ! EstablishConnection()
      } else {
        log.info("Shouldn't connect to this neurone.")
      }
    case Signal(value) =>
      log.info("Received signal = {}", value)
  }
}
