package theatre

import theatre.Genes.{ConnectionGene, NodeGene, SensorGene, OutputGene, HiddenGene, ParameterGene,
  UnknownNodeGene}

import scala.util.Sorting.stableSort

object GenomeOperations {
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
                  case (None, Some(_)) =>
                    throw UnknownNodeGene("Unknown connection input.")
                  case (Some(_), None) =>
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
