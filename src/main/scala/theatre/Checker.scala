package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import theatre.Brain.CheckStemCells
import theatre.VectorTools._
import theatre.Neurone._

object Checker {
  def props(
             neuroneCoordinates: Point,
             inputRadius: Double,
             processingFunction: Double => Double,
             firingThreshold: Double
           ): Props =
    Props(new Checker(neuroneCoordinates, inputRadius, processingFunction, firingThreshold))
}

class Checker(
               neuroneCoordinates: Point,
               inputRadius: Double,
               processingFunction: Double => Double,
               firingThreshold: Double
             ) extends Actor with ActorLogging {

  var neuroneState: List[(ActorRef, Double)] = List()

  def receive: Receive = {
    case Signal(value) =>
      log.info(s"Signal = $value received.")

      neuroneState = updateNeuroneState(value, sender(), neuroneState)

      val wasFired =
        shouldFire(
          neuroneState.map{ case (_: ActorRef, lastSignal: Double) => lastSignal},
          firingThreshold,
          processingFunction
        )

      if(wasFired) {
        log.info("Checker was fired. Check stem cells.")
        context.parent ! CheckStemCells()
        neuroneState = Nil
      } else {
          log.info("Checker neurone wasn't fired.")
      }

    case LookingForConnections(pos, axonCoor) =>
      if (segmentIntersectsBall(pos, add(pos,axonCoor), neuroneCoordinates, inputRadius) && sender != self) {
        log.info("New input neurone:" + sender + " Sending my coordinates.")
        sender() ! EstablishConnection()
      } else {
        log.info("Shouldn't connect to this neurone")
      }

    case LookForConnections() =>
  }
}

