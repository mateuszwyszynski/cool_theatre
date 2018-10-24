package theatre

import akka.actor._
import theatre.Brain._
import theatre.CPPN.StemCellReadyToUse
import theatre.Neurone.{LookForConnections, LookingForConnections}
import theatre.Stem.ReportStatus
import theatre.VectorTools._

object Brain {
  def props(
             genome: Genome,
             boundaryFunction: Point => Boolean
           ): Props =
    Props(new Brain(genome, boundaryFunction))

  def thereIsNeuroneAt(
                       position: Point,
                       existingNeurones: List[(Double, Double, Double, Double)]
                     ): Boolean = {
    existingNeurones.map(x => (x._1, x._2, x._3)).contains(position)
  }

  final case class Create(
                           cell: CellType,
                           stemCellID: String
                         )

  final case class CreateStem(position: Point, resources: Double, stemID: String)

  final case class CheckStemCells()

  final case class KillCPPNQuery(stemCellID: String)
}

class Brain(
             genome: Genome,
             boundaryFunction: Point => Boolean
           ) extends Actor with ActorLogging {
  var stemCellToActorRef: Map[String, ActorRef] = Map.empty[String, ActorRef]

  var takenPositions: List[Point] = Nil

  var stemCellNumber: BigInt = 1

  val brainsCPPN: ActorRef = context.actorOf(CPPN.props(boundaryFunction, genome))

  override def receive: Receive = {
    case msg @ Create(cell, stemCellID) =>
      cell match {
        case stem: StemCell =>
          val stemID = "StemCell" + stemCellNumber
          stemCellNumber += 1
          val stemCellActor = context.actorOf(Stem.props(stem.position, stem.neuroneMaterial, stemID), stemID)

          log.info("Created " + stemID)

          stemCellToActorRef += stemID -> stemCellActor
        case NeuroneCell(_, _, _, _, _) =>
          stemCellToActorRef.get(stemCellID) match {
            case Some(stem) => stem ! msg
            case None => log.info("Unknown stem cell ID.")
          }
        case OutputCell(_, _, _) =>
          stemCellToActorRef.get(stemCellID) match {
            case Some(stem) => stem ! msg
            case None => log.info("Unknown stem cell ID.")
          }
        case CheckerCell(_, _, _, _) =>
          stemCellToActorRef.get(stemCellID) match {
            case Some(stem) => stem ! msg
            case None => log.info("Unknown stem cell ID.")
          }
      }

    case msg: LookingForConnections =>
      for {
        stemCellActor <- stemCellToActorRef.values
      } yield {
        stemCellActor.forward(msg)
      }

    case msg: LookForConnections =>
      for {
        stemCellActor <- stemCellToActorRef.values
      } yield {
        stemCellActor ! msg
      }

    case msg @ StemCellReadyToUse(_, _, _) =>
      brainsCPPN ! msg

    case CheckStemCells() =>
      for {
        stemCellActor <- stemCellToActorRef.values
      } yield {
        stemCellActor ! ReportStatus()
      }

    case msg: KillCPPNQuery =>
      brainsCPPN ! msg

    case CreateStem(position, resources, stemID) =>
      val stemActor: ActorRef = context.actorOf(Stem.props(position, resources, stemID), stemID)

      stemCellToActorRef += stemID -> stemActor
  }
}
