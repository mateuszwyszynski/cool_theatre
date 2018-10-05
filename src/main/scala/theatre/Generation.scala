package theatre

import akka.actor.{Actor, ActorLogging, Props}
import theatre.Generation.{Evaluate, Evaluated}
import theatre.VectorTools.{Point, sortByReward}

object Generation {
  def props(generationNumber: Int): Props = Props(new Generation(generationNumber))

  final case class Evaluate(genomes: List[Genome], boundaryFunction: Point => Boolean)
  final case class Evaluated(rewards: List[(Genome, Double)])
}

class Generation(generationNumber: Int) extends Actor with ActorLogging {
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
