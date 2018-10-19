package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import gym.scala.client.GymSpace.{GymInstance, Observation, StepReply}
import gym.scala.client.{createEnv, gymClient, resetEnv, step}
import theatre.Brain.{Create, CreateStem}
import theatre.Neurone.{LookingForConnections, Signal}
import theatre.VectorTools.{cubeInterior, sigmoidalFunction}
import theatre.Reality._

object Reality {
  def props(envID: String): Props = Props(new Reality(envID))

  final case class Observer(genome: Genome, brainID: String)
  final case class ObserverNoticed(brainID: String)
  final case object ObserverLeaving
  final case object ResetEnvironment
  final case class Action(value: Int)
}

class Reality(envID: String) extends Actor with ActorLogging {
  val environment: GymInstance = gymClient.execute(createEnv(envID))

  def receive: Receive = {
    case Observer(genome, brainID) =>
      val observer: ActorRef = context.actorOf(Brain.props(genome, cubeInterior), brainID)

      observer ! CreateStem((0.5, 0.5, 0.0), 1, "Hands")

      observer ! Create(OutputCell((0.5, 0.5, -0.1), 0.6, self), "Hands")

      observer ! Create(StemCell((0.5, 0.5, 0.5), 100), "StemCell1")

      observer ! Create(NeuroneCell((0.5, 0.5, 0.5), (0.0, 0.0, -0.3), 0.7, sigmoidalFunction, 0), "StemCell1")

      val eye1 =
        context.actorOf(Neurone.props((0.25, 0.25, 1.1), (0.0, 0.0, -0.3), 0.5, sigmoidalFunction, 0), "Eye1")

      val eye2 =
        context.actorOf(Neurone.props((0.25, 0.75, 1.1), (0.0, 0.0, -0.3), 0.5, sigmoidalFunction, 0), "Eye2")

      val eye3 =
        context.actorOf(Neurone.props((0.75, 0.25, 1.1), (0.0, 0.0, -0.3), 0.5, sigmoidalFunction, 0), "Eye3")

      val eye4 =
        context.actorOf(Neurone.props((0.75, 0.75, 1.1), (0.0, 0.0, -0.3), 0.5, sigmoidalFunction, 0), "Eye4")

      val eyes: List[ActorRef] = List(eye1, eye2, eye3, eye4)

      observer.tell(LookingForConnections((0.25, 0.25, 1.1), (0.0, 0.0, -0.3)), eye1)
      observer.tell(LookingForConnections((0.25, 0.75, 1.1), (0.0, 0.0, -0.3)), eye2)
      observer.tell(LookingForConnections((0.75, 0.25, 1.1), (0.0, 0.0, -0.3)), eye3)
      observer.tell(LookingForConnections((0.75, 0.75, 1.1), (0.0, 0.0, -0.3)), eye4)

      context.become(awaitingEvaluation(eyes, observer))

      sender ! ObserverNoticed(brainID)
  }

  def awaitingEvaluation(eyes: List[ActorRef], brain: ActorRef): Receive = {
    case ResetEnvironment =>
      log.info("Resetting the Matrix " + sender)
      val response: Observation = gymClient.execute(resetEnv(environment))

      for {
        x <- response.observation zip eyes
      } yield {
        x._2 ! Signal(x._1)
      }

      context.become(evaluating(eyes, brain, sender))
  }

  def evaluating(eyes: List[ActorRef], brain: ActorRef, scoreSupervisor: ActorRef): Receive = {
    case Action(value: Int) =>
      val response: StepReply = gymClient.execute(step(environment, value, render = true))

      if(response.done) {
        brain ! PoisonPill
        for {
          eye <- eyes
        } yield {
          log.info("poisoned eye")
          eye ! PoisonPill
        }

        scoreSupervisor ! response.reward

        context.become(receive)
      } else {
        val inputs: List[Double] = response.observation

        for {
          x <- inputs zip eyes
        } yield {
          x._2 ! Signal(x._1)
        }
      }
  }
}
