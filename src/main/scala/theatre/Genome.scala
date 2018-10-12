package theatre

import Genes._
import VectorTools.{crossOverGenomesAt, determineNodes}

object Genome {
  def apply(nodeGenes: Map[Int, NodeGene], connectionGenes: List[ConnectionGene]): Genome =
    new Genome(nodeGenes, connectionGenes)
}

case class Genome(nodeGenes: Map[Int, NodeGene], connectionGenes: List[ConnectionGene]) {
  val activeConnections: List[ConnectionGene] = connectionGenes.filter(x => x.enabled)

  val numberOfConnections: Int = connectionGenes.length
  val numberOfActiveConnections: Int = activeConnections.length

  val numberOfNodes: Int = nodeGenes.keys.max

  private val randomizer = scala.util.Random

  def nodes(): Map[Int, NodeGene] = nodeGenes
  def connections(): List[ConnectionGene] = connectionGenes

  private def findConnection(edge: (Int, Int), shouldBeActive: Boolean): Option[ConnectionGene] = {
    def findConnectionAcc(edge: (Int, Int), connectionGenes: List[ConnectionGene]): Option[ConnectionGene] =
      connectionGenes match {
        case x :: xs if shouldBeActive =>
          if(x.input == edge._1 && x.output == edge._2 && x.enabled) {
            Some(x)
          } else {
            findConnectionAcc(edge, xs)
          }
        case x :: xs =>
          if(x.input == edge._1 && x.output == edge._2 && !x.enabled) {
            Some(x)
          } else {
            findConnectionAcc(edge, xs)
          }
        case Nil => None
      }

    findConnectionAcc(edge, connectionGenes)
  }

  def addNodeAtConnection(input: Int, output: Int, baseValue: Double): Genome =
    findConnection((input, output), shouldBeActive = true) match {
    case Some(c) =>
      val createdNode: NodeGene = HiddenGene(baseValue)

      val newNodeGenes: Map[Int, NodeGene] = nodeGenes.updated(numberOfNodes + 1, createdNode)

      val incomingConnection: ConnectionGene =
        ConnectionGene(
          c.input,
          numberOfNodes + 1,
          1,
          enabled = true
        )

      val outgoingConnection: ConnectionGene =
        ConnectionGene(
          numberOfNodes + 1,
          c.output,
          c.weight,
          enabled = true
        )

      val newConnectionGenes: List[ConnectionGene] =
        connectionGenes.map(x => x.disableIfLike(c)) :::
          List(incomingConnection, outgoingConnection)

      Genome(newNodeGenes, newConnectionGenes)

    case None => this
  }

  def generateNewNode(): Genome = {
    val connectionToSplitIndex: Int = randomizer.nextInt(numberOfActiveConnections)

    val connectionToSplit: ConnectionGene = activeConnections(connectionToSplitIndex)

    addNodeAtConnection(connectionToSplit.input, connectionToSplit.output, randomizer.nextDouble())
  }

  def addConnectionBetween(input: Int, output: Int, weight: Double): Genome = {
    findConnection((input, output), shouldBeActive = false) match {
      case Some(c) =>
        if (c.enabled) {
          Genome(nodeGenes, connectionGenes :+ ConnectionGene(input, output, weight, enabled = true))
        } else {
          val disabledConnectionIndex: Int =
            connectionGenes.indexWhere(x => !x.enabled && x.input == c.input && x.output == c.output)

          val connections: List[ConnectionGene] =
            (connectionGenes.take(disabledConnectionIndex) :+
            ConnectionGene(input, output, weight, enabled = true)) :::
            connectionGenes.drop(disabledConnectionIndex + 1)

          val updatedNodeGenes: Map[Int, NodeGene] =
            determineNodes(connections, Map.empty[Int, NodeGene], nodeGenes)

          Genome(updatedNodeGenes, connections)
        }
      case None =>
        (nodeGenes.get(input), nodeGenes.get(output)) match {
          case (Some(_), Some(_)) =>
            val connections: List[ConnectionGene] =
              connectionGenes :+ ConnectionGene(input, output, weight, enabled = true)

            val updatedNodeGenes: Map[Int, NodeGene] =
              determineNodes(connections, Map.empty[Int, NodeGene], nodeGenes)

            Genome(updatedNodeGenes, connections)
          case (None, Some(_)) =>
            throw WrongConnectionGene("No node found within the genome for the specified input.")
          case (Some(_), None) =>
            throw WrongConnectionGene("No node found within the genome for the specified output.")
          case _ =>
            throw WrongConnectionGene("No nodes found within the genome for both input and output.")
        }
    }
  }

  def generateNewConnection(): Genome = {
    if(numberOfActiveConnections == numberOfNodes*(numberOfNodes-1)/2) {
      this.generateNewNode()
    } else {
      val nodeNumbers = 1 to numberOfNodes

      val fullGraph: Seq[(Int, Int)] = nodeNumbers.flatMap{ y =>
        for {
          x <- 1 to numberOfNodes if x != y
        } yield {
          (y, x)
        }
      }

      def alreadyHasThisGene(edge: (Int, Int), connectionGenes: List[ConnectionGene]): Boolean =
        connectionGenes match {
          case x :: xs =>
            (x.input == edge._1 && x.output == edge._2) || alreadyHasThisGene(edge, xs)
          case Nil => false
        }

      val possibleNewConnections: Seq[(Int, Int)] =
        fullGraph.filter(x => !alreadyHasThisGene(x, connectionGenes))

      val newConnectionIndex: Int = randomizer.nextInt(possibleNewConnections.length)

      val newConnection: (Int, Int) = possibleNewConnections(newConnectionIndex)

      val presentConnGene: Option[ConnectionGene] =
        connectionGenes.find(x => x.input == newConnection._1 && x.output == newConnection._2)

      val newConnectionGenes: List[ConnectionGene] = presentConnGene match {
        case Some(cg) => connectionGenes.map(x => x.disableIfLike(cg))
        case None => connectionGenes :+ new ConnectionGene(
          newConnection._1,
          newConnection._2,
          weight = randomizer.nextDouble(),
          enabled = true
        )
      }

      val updatedNodes: Map[Int, NodeGene] = nodeGenes.get(newConnection._2) match {
        case Some(node) => node match {
          case ParameterGene(b) => nodeGenes.updated(newConnection._2, HiddenGene(b))
          case _ => nodeGenes
        }
        case None =>
          println("Unexpected behaviour: connection output is an unknown node.")
          nodeGenes
      }

      Genome(updatedNodes, newConnectionGenes)
    }
  }

  def mateWith(genome: Genome): Genome = {
    val split: Int = randomizer.nextInt(this.numberOfConnections + genome.numberOfConnections + 1)

    crossOverGenomesAt(this, genome, split)
  }
}