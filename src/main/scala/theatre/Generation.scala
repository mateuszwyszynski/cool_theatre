package theatre

import akka.actor.{Actor, ActorLogging, Props}
import theatre.Generation.{Evaluate, Evaluated}
import theatre.VectorTools.Point

object Generation {
  def props(generationNumber: Int): Props = Props(new Generation(generationNumber))

  final case class Evaluate(genomes: List[Genome], boundaryFunction: Point => Boolean)
  final case class Evaluated(rewards: List[Double])
}

class Generation(generationNumber: Int) extends Actor with ActorLogging {
  def receive: Receive = {
    case Evaluate(genomes, boundaryFunction) =>
      val collectedRewards: List[Double] =
        for {
          genome <- genomes
        } yield {
          val brain = context.actorOf(Brain.props(genome, boundaryFunction))

          1.0
        }

      context.parent ! Evaluated(collectedRewards)
  }
}
