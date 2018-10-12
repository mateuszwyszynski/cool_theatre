package theatre

import akka.actor.{Actor, ActorLogging, Props}
import theatre.Generation.{Evaluate, Evaluated}
import theatre.VectorTools.Point
import theatre.GenomeOperations.sortByReward

object Generation {
  def props(): Props = Props(new Generation())

  final case class Evaluate(genomes: List[Genome], boundaryFunction: Point => Boolean)
  final case class Evaluated(rewards: List[(Genome, Double)])
}

class Generation() extends Actor with ActorLogging {
  def receive: Receive = {
    case Evaluate(genomes, boundaryFunction) =>
      val evaluatedGenomes: List[(Genome, Double)] =
        for {
          genome <- genomes
        } yield {
          val brain = context.actorOf(Brain.props(genome, boundaryFunction))
          (genome, 1.0)
        }

      val genomesSortedByReward: List[(Genome, Double)] =
        sortByReward(evaluatedGenomes)

      context.parent ! Evaluated(genomesSortedByReward.take(10))
  }
}
