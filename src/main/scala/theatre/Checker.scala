package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import theatre.Brain.CheckStemCells
import theatre.VectorTools.{Point, segmentIntersectsBall, add}
import theatre.Neurone.{Signal, shouldFire, EstablishConnection, LookForConnections, LookingForConnections,
  updateNeuroneState}

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
      neuroneState = updateNeuroneState(value, sender(), neuroneState)

      val wasFired =
        shouldFire(
          neuroneState.map{ case (_: ActorRef, lastSignal: Double) => lastSignal},
          firingThreshold,
          processingFunction
        )

      if(wasFired) {
        context.parent ! CheckStemCells()
        neuroneState = Nil
      } else {}

    case LookingForConnections(pos, axonCoor) =>
      if (segmentIntersectsBall(pos, add(pos,axonCoor), neuroneCoordinates, inputRadius) && sender != self) {
        sender() ! EstablishConnection()
      } else {}

    case LookForConnections() =>
  }
}

