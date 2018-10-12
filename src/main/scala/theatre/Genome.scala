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

  def mutateRandomNode(): Genome = {
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

  def mutateRandomConnection(): Genome = {
    val nodeNumbers = 1 to numberOfNodes

    val fullGraph: Seq[(Int, Int)] = nodeNumbers.flatMap{ y =>
      for {
        x <- 1 to numberOfNodes if x != y
      } yield {
        (y, x)
      }
    }

    val randomConnectionIndex: Int = randomizer.nextInt(fullGraph.length)

    val randomlyDrawnConnection: (Int, Int) = fullGraph(randomConnectionIndex)

    this.addConnectionBetween(randomlyDrawnConnection._1, randomlyDrawnConnection._2, randomizer.nextDouble())
  }

  def mateWith(genome: Genome): Genome = {
    val split: Int = randomizer.nextInt(this.numberOfConnections + genome.numberOfConnections + 1)

    crossOverGenomesAt(this, genome, split)
  }
}