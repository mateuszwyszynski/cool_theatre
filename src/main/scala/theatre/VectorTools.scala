package theatre

import Genes._

import scala.util.Sorting.stableSort

object VectorTools {
  type Point = (Double, Double, Double)

  def segmentIntersectsBall(
                             segmentBeginning: Point,
                             segmentEnd: Point,
                             ballCenter: Point,
                             radius: Double
                           ): Boolean = {
    val projectionOfOutputPos: Point =
      projectOnSegment(segmentBeginning, segmentEnd, ballCenter)

    norm(subtract(projectionOfOutputPos, ballCenter)) <= radius
  }

  def projectOnSegment(
                     segmentBeginning: Point,
                     segmentEnd: Point,
                     point: Point
                   ): Point = {
    val distanceToBeg = norm(subtract(point, segmentBeginning))
    val distanceToEnd = norm(subtract(point, segmentEnd))

    val closerEnd =
      if(distanceToBeg <= distanceToEnd) segmentBeginning else segmentEnd

    val projOnDir = projectOnDirection(segmentBeginning, segmentEnd, point)
    val segmentLength = norm(subtract(segmentEnd, segmentBeginning))

    val distProjToBeg = norm(subtract(projOnDir, segmentBeginning))
    val distProjToEnd = norm(subtract(projOnDir, segmentEnd))

    val projection =
      if(Math.max(distProjToBeg, distProjToEnd) <= segmentLength) projOnDir else closerEnd

    projection
  }

  def projectOnDirection(
                        segmentBeginning: Point,
                        segmentEnd: Point,
                        point: Point
                      ): Point = {
    val normalizedDirection: Point = normalize(subtract(segmentEnd, segmentBeginning))
    val length: Double = scalarProduct(normalizedDirection, subtract(point, segmentBeginning))
    val linearProj = scalarMultiplication(length, normalizedDirection)
    add(segmentBeginning, linearProj)
  }

  def add(
                 tuple1: Point,
                 tuple2: Point
               ): Point = {
    (tuple1._1 + tuple2._1, tuple1._2 + tuple2._2, tuple1._3 + tuple2._3)
  }

  def scalarMultiplication(
                          scalar: Double,
                          tuple: Point
                          ): Point ={
    (scalar * tuple._1, scalar * tuple._2, scalar * tuple._3)
  }

  def subtract(
           tuple1: Point,
           tuple2: Point
         ): Point = {
    (tuple1._1 - tuple2._1, tuple1._2 - tuple2._2, tuple1._3 - tuple2._3)
  }

  def normalize(tuple: Point): Point = {
    val length = norm(tuple)
    (tuple._1/length, tuple._2/length, tuple._3/length)
  }

  def norm(tuple: Point): Double =
    Math.sqrt(scalarProduct(tuple, tuple))

  def scalarProduct(
                     tuple1: Point,
                     tuple2: Point
                   ): Double = {
    tuple1._1 * tuple2._1 + tuple1._2 * tuple2._2 + tuple1._3 * tuple2._3
  }

  def tensorProduct(
                     tuple1: Point,
                     tuple2: Point
                   ): Point = {
    (tuple1._1 * tuple2._1, tuple1._2 * tuple2._2, tuple1._3 * tuple2._3)
  }

  def sigmoidalFunction(x: Double): Double = 1 / (1 + math.exp(-x))

  def createBaseNodes(numberOfInputs: Int, numberOfOutputs: Int): Map[Int, NodeGene] = {
    def baseNodesAcc(
                      inputs: List[Int],
                      outputs: List[Int],
                      acc: Map[Int, NodeGene]
                    ): Map[Int, NodeGene] = inputs match {
      case Nil => outputs match {
        case Nil => acc
        case x :: xs =>
          baseNodesAcc(Nil, xs, acc.updated(x, OutputGene(0.0)))
      }
      case x :: xs =>
        baseNodesAcc(xs, outputs, acc.updated(x, SensorGene()))
    }

    baseNodesAcc(
      (1 to numberOfInputs).toList,
      (numberOfInputs + 1 to numberOfInputs + numberOfOutputs).toList,
      Map.empty[Int, NodeGene]
    )
  }

  def createConnections(numberOfInputs: Int, numberOfOutputs: Int): List[ConnectionGene] =
    for {
      input <- (1 to numberOfInputs).toList
      output <- (numberOfInputs + 1 to numberOfInputs + numberOfOutputs).toList
    } yield {
      ConnectionGene(input, output, 1.0, enabled = true)
    }

  def sortByReward(evaluatedGenomes: List[(Genome, Double)]): List[(Genome, Double)] =
    stableSort(evaluatedGenomes, (e1: (Genome, Double), e2: (Genome, Double)) => e1._2 > e2._2).toList

  def determineNodes(
                      connections: List[ConnectionGene],
                      knownNodes: Map[Int, NodeGene],
                      parentNodes: Map[Int, NodeGene]
                    ): Map[Int, NodeGene] = {
    def determineFromEnabledAccumulator(
                              connections: List[ConnectionGene],
                              parentNodes: Map[Int, NodeGene],
                              acc: Map[Int, NodeGene]
                            ): Map[Int, NodeGene] =
      connections match {
        case Nil => acc
        case x :: xs =>
          val accWithUpdatedInput: Map[Int, NodeGene] =
            acc.get(x.input) match {
            case Some(_) => acc
            case None =>
              parentNodes.get(x.input) match {
                case Some(node) =>
                  node match {
                  case OutputGene(b) =>
                    acc.updated(x.input, ParameterGene(b))
                  case HiddenGene(b) =>
                    acc.updated(x.input, ParameterGene(b))
                  case ParameterGene(b) =>
                    acc.updated(x.input, ParameterGene(b))
                  case SensorGene() =>
                    acc.updated(x.input, SensorGene())
                  }
                case None =>
                  println("Unexpected behaviour at input {}: unknown NodeGene.", x.input)
                  acc
              }
          }

          val accWithUpdatedOutput: Map[Int, NodeGene] = accWithUpdatedInput.get(x.output) match {
            case Some(node) => node match {
              case ParameterGene(base) => accWithUpdatedInput.updated(x.output, HiddenGene(base))
              case _ => accWithUpdatedInput
            }
            case None =>
              parentNodes.get(x.output) match {
                case Some(node) => node match {
                  case o: OutputGene =>
                    accWithUpdatedInput.updated(x.output, o)
                  case h: HiddenGene =>
                    accWithUpdatedInput.updated(x.output, h)
                  case ParameterGene(b) =>
                    accWithUpdatedInput.updated(x.output, HiddenGene(b))
                  case s: SensorGene =>
                    println("Unexpected behaviour (?): sensor gene treated as output.")
                    accWithUpdatedInput.updated(x.output, s)
                }
                case None =>
                  println("Unexpected behaviour at output {}: yet unknown NodeGene.", x.output)
                  accWithUpdatedInput
              }
          }

          determineFromEnabledAccumulator(xs, parentNodes, accWithUpdatedOutput)
      }

    val first = determineFromEnabledAccumulator(connections.filter(c => c.enabled), parentNodes, knownNodes)

    def determineFromDisabledAccumulator(
                                          connections: List[ConnectionGene],
                                          parentNodes: Map[Int, NodeGene],
                                          acc: Map[Int, NodeGene]
                                        ): Map[Int, NodeGene] =
      connections match {
        case Nil => acc
        case c :: cs =>
          val accUpdated: Map[Int, NodeGene] =
            (acc.get(c.input), acc.get(c.output)) match {
              case (Some(_), Some(_)) => acc
              case (None, Some(_)) =>
                parentNodes.get(c.input) match {
                  case Some(n) => acc.updated(c.input, n)
                  case None =>
                    throw UnknownNodeGene("Unknown connection input.")
                }
              case (Some(_), None) =>
                parentNodes.get(c.output) match {
                  case Some(n) => acc.updated(c.output, n)
                  case None =>
                    throw UnknownNodeGene("Unknown connection output.")
                }
              case (None, None) =>
                (parentNodes.get(c.input), parentNodes.get(c.output)) match {
                  case (Some(i), Some(o)) => acc.updated(c.input, i).updated(c.output, o)
                  case (None, Some(o)) =>
                    throw UnknownNodeGene("Unknown connection input.")
                  case (Some(i), None) =>
                    throw UnknownNodeGene("Unknown connection output.")
                  case (None, None) =>
                    throw UnknownNodeGene("Unknown connection input and output")
                }
            }

          determineFromDisabledAccumulator(cs, parentNodes, accUpdated)
      }

    determineFromDisabledAccumulator(connections.filter(!_.enabled), parentNodes, first)
  }

  def crossOverGenomesAt(genome1: Genome, genome2: Genome, cross: Int): Genome = {
    if(cross >= genome1.connections().length + genome2.connections().length + 1) {
      throw new IndexOutOfBoundsException("Trying to cross over genomes after the genomes have ended.")
    } else {
      val (parent1, parent2, split): (Genome, Genome, Int) =
        if(cross <= genome1.numberOfConnections) {
          (genome1, genome2, cross)
        } else {
          (genome2, genome1, cross - genome1.connections().length)
        }


      val headConnections: List[ConnectionGene] = parent1.connections().take(split)

      val tailConnections: List[ConnectionGene] = parent2.connections().drop(split)

      Genome(
        determineNodes(
          tailConnections.filter(x => x.enabled),
          determineNodes(
            headConnections.filter(x => x.enabled),
            Map.empty[Int, NodeGene],
            parent1.nodes()
          ),
          parent2.nodes()
        ),
        headConnections ::: tailConnections
      )
    }
  }
}
