package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import theatre.Generation.{Evaluate, Evaluated}
import VectorTools.Point

object Evolution {
  def props(boundaryFunction: Point => Boolean, goal: Double): Props =
    Props(new Evolution(boundaryFunction, goal))
}

class Evolution(boundaryFunction: Point => Boolean, goal: Double) extends Actor with ActorLogging {
  def receive: Receive = {
    case Evaluated(genomesWithRewards) =>
      sender ! PoisonPill

      genomesWithRewards.find(y => y._2 >= goal) match {
        case Some(g) =>
          log.info(g._1 + " has accomplished desired goal")
        case None =>
          val nextGeneration: ActorRef = context.actorOf(Generation.props())

          val parents: List[Genome] = genomesWithRewards.map(x => x._1)

          val children: List[Genome] = for {
            parent1 <- parents
            parent2 <- parents
          } yield {
            parent1.mateWith(parent2)
          }
          nextGeneration ! Evaluate(children, boundaryFunction)
      }
  }
}
