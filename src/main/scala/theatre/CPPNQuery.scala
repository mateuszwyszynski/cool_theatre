package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import theatre.Brain.Create
import theatre.CPPN.StemCellReadyToUse
import theatre.CPPNQuery.Result
import theatre.ParameterNode.Parameter
import theatre.SensorNode.{AddOutputNode, Input}
import theatre.VectorTools.{Point, sigmoidalFunction}
import Genes._

object CPPNQuery {
  def props(genome: Genome, stemCellID: String): Props =
    Props(new CPPNQuery(genome, stemCellID))

  final case class StemCellReadyToUse(position: Point, resources: Double, stemCellID: String)
  final case class Result(value: Double)
}

class CPPNQuery(
                 genome: Genome,
                 stemCellID: String
               )
  extends Actor
    with ActorLogging {
  var collectedOutputs: Map[Int, Double] = Map.empty[Int, Double]

  def createActorNodeFromGene(gene: NodeGene): (ActorRef, NodeGene) = gene match {
    case SensorGene() =>
      (context.actorOf(SensorNode.props(sigmoidalFunction)), gene)
    case OutputGene(b) =>
      (context.actorOf(OutputNode.props(sigmoidalFunction, sigmoidalFunction(b))), gene)
    case HiddenGene(b) =>
      (context.actorOf(HiddenNode.props(sigmoidalFunction, sigmoidalFunction(b))), gene)
    case ParameterGene(b) =>
      (context.actorOf(ParameterNode.props(sigmoidalFunction, sigmoidalFunction(b))), gene)
  }

  var nodes: Map[Int, (ActorRef, NodeGene)] = Map.empty[Int, (ActorRef, NodeGene)]

  val actors: Iterable[ActorRef] = for {
    gene <- genome.nodes()
  } yield {
    val newNode = gene._2 match {
      case SensorGene() =>
        context.actorOf(SensorNode.props(sigmoidalFunction),
          "Senor" + gene._1)
      case OutputGene(b) =>
        context.actorOf(OutputNode.props(sigmoidalFunction, sigmoidalFunction(b)),
          "Output" + gene._1)
      case HiddenGene(b) =>
        context.actorOf(HiddenNode.props(sigmoidalFunction, sigmoidalFunction(b)),
          "Hidden" + gene._1)
      case ParameterGene(b) =>
        context.actorOf(ParameterNode.props(sigmoidalFunction, sigmoidalFunction(b)),
          "Parameter" + gene._1)
    }

    nodes += gene._1 -> (newNode, gene._2)

    newNode
  }

  val sensorNodes: Map[Int, (ActorRef, NodeGene)] =
    nodes.filter(x => x._2._2.isInstanceOf[SensorGene])

  val parameterNodes: Map[Int, (ActorRef, NodeGene)] =
    nodes.filter(x => x._2._2.isInstanceOf[ParameterGene])

  val outputNodesNumbers: Map[ActorRef, Int] =
    nodes.filter(x => x._2._2.isInstanceOf[OutputGene]).mapValues(_._1).map(_.swap)

  val simpleMap: Map[Int, String] = Map((1, "one"), (2, "two"), (3, "three"), (4, "four"))

  for {
    conn <- genome.connections()
  } yield {
    nodes(conn.input)._1 ! AddOutputNode(nodes(conn.output)._1, conn.weight)
  }

  def receive: Receive = {
    case StemCellReadyToUse(position, resources, stemID) =>
      if(resources <= 0) {
        log.info(s"No more resources in the stem cell $stemID")
      } else {
        sensorNodes(1)._1 ! Input(position._1)
        sensorNodes(2)._1 ! Input(position._2)
        sensorNodes(3)._1 ! Input(position._3)
        sensorNodes(4)._1 ! Input(resources)

        for {
          parNode <- parameterNodes.values.map(x => x._1)
        } yield {
          parNode ! Parameter()
        }
      }
    case Result(v) =>
      collectedOutputs = outputNodesNumbers.get(sender) match {
        case Some(n) => n match {
          case 5 => collectedOutputs.updated(5, v)
          case 6 => collectedOutputs.updated(6, (v-0.5)/(0.5) * 1)
          case 7 => collectedOutputs.updated(7, (v-0.5)/(0.5) * 1)
          case 8 => collectedOutputs.updated(8, (v-0.5)/(0.5) * 1)
          case 9 => collectedOutputs.updated(9, (v-0.5)/(0.5) * 1)
          case 10 => collectedOutputs.updated(10, (v-0.5)/(0.5) * 2 - 1)
          case 11 => collectedOutputs.updated(11, (v-0.5)/(0.5) * 2 - 1)
          case 12 => collectedOutputs.updated(12, (v-0.5)/(0.5) * 2 - 1)
          case 13 => collectedOutputs.updated(13, v)
          case 14 => collectedOutputs.updated(14, (v-0.5)/(0.5) * 1)
          case 15 => collectedOutputs.updated(15, v)
          case _ =>
            log.info("Error: to many output nodes on the list, {}", sender)
            collectedOutputs
        }
        case None =>
          log.info("Error: unknown output node {}", sender)
          collectedOutputs
      }

      if(collectedOutputs.keys.toList.length == 11) {
        val position: Point =
          (collectedOutputs(6), collectedOutputs(7), collectedOutputs(8))
        val inpRad: Double = collectedOutputs(9)
        val axCoor: Point =
          (collectedOutputs(10), collectedOutputs(11), collectedOutputs(12))
        val procFun: Double => Double = sigmoidalFunction
        val threshold: Double = collectedOutputs(14)
        if(collectedOutputs(5) <= 0.75) {
          log.info("New neurone cell - pos: {}, ax: {}, inpR: {}, thr: {}", position,axCoor,inpRad,threshold)
          context.parent ! Create(NeuroneCell(position, axCoor, inpRad, procFun, threshold), stemCellID)
        } else {
          log.info("New checker cell - pos: {}, ax: {}, inpR: {}, thr: {}", position,axCoor,inpRad,threshold)
          context.parent ! Create(CheckerCell(position, inpRad, procFun, threshold), stemCellID)
        }
        collectedOutputs = Map.empty[Int, Double]
      } else {}
  }
}

