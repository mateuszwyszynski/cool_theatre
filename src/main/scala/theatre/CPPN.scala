package theatre

import akka.actor._
import theatre.Brain.Create
import theatre.CPPN._
import theatre.VectorTools._

object CPPN {
  def props(boundaryFunction: Point => Boolean, genome: Genome): Props =
    Props(new CPPN(boundaryFunction, genome))

  final case class StemCellReadyToUse(position: Point, resources: Double, stemCellID: String)
}

class CPPN(
            boundaryFun: Point => Boolean,
            genome: Genome
          )
  extends Actor
  with ActorLogging {
  def receive: Receive = {
    case msg @ StemCellReadyToUse(_, resources, stemCellID) =>
      if(resources <= 0) {
        log.info(s"No more resources in the stem cell $stemCellID")
      } else {
        log.info("Creating query actor")
        val queryActor: ActorRef =
          context.actorOf(CPPNQuery.props(genome, stemCellID), "CPPNQueryFor" + stemCellID)

        queryActor ! msg
      }
    case msg: Create =>
      context.parent ! msg
  }
}