package theatre

import akka.actor.{Props, Actor, ActorRef, ActorLogging}
import theatre.VectorTools.{Point, segmentIntersectsBall, add}

object Neurone {
  def props(
             neuroneCoordinates: Point,
             axonCoordinates: Point,
             inputRadius: Double,
             processingFunction: Double => Double,
             firingThreshold: Double
           ): Props =
    Props(new Neurone(neuroneCoordinates, axonCoordinates, inputRadius, processingFunction, firingThreshold))

  def shouldFire(neuroneState : List[Double], threshold: Double, processingFun: Double => Double): Boolean = {
    processingFun(neuroneState.sum) >= threshold
  }

  def updateNeuroneState(
                          signalValue: Double,
                          sender: ActorRef,
                          neuroneState: List[(ActorRef, Double)]
                        ): List[(ActorRef, Double)] = {
    (sender, signalValue) :: neuroneState.filter(x => x._1 != sender)
  }

  def propagateSignalToOutputNeurones(
                                       outputNeurones: List[ActorRef],
                                       signalToPropagate: Double
                                     ): List[ActorRef] =  outputNeurones match {
    case x :: xs =>
      x ! Signal(signalToPropagate)
      propagateSignalToOutputNeurones(xs, signalToPropagate)
    case Nil => Nil
  }

  final case class Signal(value: Double)
  final case class LookForConnections()
  final case class LookingForConnections(
                                          position: Point,
                                          axonCoordinates: Point
                                        )
  final case class EstablishConnection()
}

class Neurone(
                    neuroneCoordinates: Point,
                    axonCoordinates: Point,
                    inputRadius: Double,
                    processingFunction: Double => Double,
                    firingThreshold: Double
                  ) extends Actor with ActorLogging {
  import Neurone._

  var neuroneState: List[(ActorRef, Double)] = List()

  var outputNeurones: Option[List[ActorRef]] = None

  def receive: Receive = {
    case Signal(value) =>
      neuroneState = updateNeuroneState(value, sender(), neuroneState)

      outputNeurones match {
        case Some(outNeurones) =>
          val wasFired =
            shouldFire(
              neuroneState.map{ case (_: ActorRef, lastSignal: Double) => lastSignal},
              firingThreshold,
              processingFunction
            )

          if(wasFired) {
            propagateSignalToOutputNeurones(outNeurones, processingFunction(neuroneState.map(x => x._2).sum))
            neuroneState = Nil
          } else {}
        case None =>
      }

    case LookingForConnections(pos, axonCoor) =>
    if (segmentIntersectsBall(pos, add(pos,axonCoor), neuroneCoordinates, inputRadius) && sender != self) {
      sender() ! EstablishConnection()
    } else {}

    case EstablishConnection() => outputNeurones match {
      case Some(outNeurones) =>
        outputNeurones = Some(sender() :: outNeurones)
      case None =>
        outputNeurones = Some(List(sender()))
    }

    case LookForConnections() =>
      sender ! LookingForConnections(neuroneCoordinates, axonCoordinates)
  }
}
