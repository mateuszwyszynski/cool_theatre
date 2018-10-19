package theatre

import akka.actor.{Props, Actor, ActorLogging}
import theatre.CPPN.StemCellReadyToUse
import theatre.Stem.ReportStatus
import theatre.Brain.{Create, KillCPPNQuery, CheckStemCells}
import theatre.Neurone.{LookForConnections, LookingForConnections}
import theatre.Output.EnterTheMatrix
import theatre.VectorTools.Point

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
  var checkersNumber: Int = 1
  var outputsNumber: Int = 1

  def receive: Receive = {
    case msg: EnterTheMatrix =>
      context.child("Output1") match {
        case Some(output) =>
          output ! msg
        case None =>
      }

    case Create(cell, _) =>
      if(resources <= 0) {
        context.parent ! KillCPPNQuery(stemCellID)
      } else {
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
            context.actorOf(Output.props(output.position, output.inputRadius), "Output" + outputsNumber)
            outputsNumber += 1

          case checker: CheckerCell =>
            val neuroneActor =
              context.actorOf(
                Checker.props(
                  checker.position,
                  checker.inputRadius,
                  checker.processingFunction,
                  checker.firingThreshold
                ),
                "Checker" + checkersNumber
              )

            resources -= 1

            checkersNumber += 1

            neuroneActor.tell(LookForConnections(), context.parent)
        }
      }

    case msg: LookForConnections =>
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
