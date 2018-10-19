package theatre

import VectorTools.Point
import akka.actor.ActorRef


trait CellType {
    val position: Point
}

case class NeuroneCell(
                        position: Point,
                        axonCoordinates: Point,
                        inputRadius: Double,
                        processingFunction: Double => Double,
                        firingThreshold: Double
                      ) extends CellType

case class StemCell(
                     position: Point,
                     neuroneMaterial: Double,
                   ) extends CellType

case class CheckerCell(
                        position: Point,
                        inputRadius: Double,
                        processingFunction: Double => Double,
                        firingThreshold: Double
                      ) extends CellType

case class OutputCell(
                       position: Point,
                       inputRadius: Double,
                       reality: ActorRef
                     ) extends CellType
