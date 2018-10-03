package theatre

import akka.actor._
import theatre.CPPN._
import theatre.Stem._
import theatre.Brain._
import theatre.Neurone.{LookForConnections, LookingForConnections}
import theatre.VectorTools._

object Stem{
  def props(
             position: Point,
             neuroneMaterial: Double,
             stemCellID: String
           ): Props =
    Props(new Stem(position, neuroneMaterial, stemCellID))

  final case class ReportStatus()
}

class Stem(
            position: Point,
            neuroneMaterial: Double,
            stemCellID: String
          ) extends Actor with ActorLogging {
  var resources: Double = neuroneMaterial
  var neuroneNumber: BigInt = 1

  def receive: Receive = {
    case Create(cell, _) =>
      cell match {
        case neurone: NeuroneCell =>
          val neuroneActor =
            context.actorOf(
              Neurone.props(
                neurone.position,
                neurone.axonCoordinates,
                neurone.inputRadius,
                neurone.processingFunction,
                neurone.firingThreshold
              ),
              "Neurone" + neuroneNumber
            )

          resources -= 1

          neuroneNumber += 1

          neuroneActor.tell(LookForConnections(), context.parent)

        case output: OutputCell =>
          context.actorOf(Output.props(output.position, output.inputRadius))

        case checker: CheckerCell =>
          log.info("Checker Cells are not yet implemented.")
      }

    case msg @ LookForConnections() =>
      for {
        child <- context.children
      } yield {
        child.forward(msg)
      }

    case msg: LookingForConnections =>
      for {
        child <- context.children.filter(_ != sender)
      } yield {
        child.forward(msg)
      }

    case ReportStatus() =>
      context.parent ! StemCellReadyToUse(position, resources, stemCellID)

    case msg @ CheckStemCells() => context.parent.forward(msg)
  }
}
