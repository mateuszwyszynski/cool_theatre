package theatre

import Genes._

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
}
