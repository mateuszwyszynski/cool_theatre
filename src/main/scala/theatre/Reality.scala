package theatre

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import gym.scala.client.GymSpace.{GymInstance, Observation, StepReply}
import gym.scala.client.{createEnv, gymClient, resetEnv, step}
import theatre.Neurone.Signal
import theatre.Reality.{Action, Observer, ResetEnvironment}

object Reality {
  def props(envID: String): Props = Props(new Reality(envID))

  final case class Observer(eyes: List[ActorRef])
  final case object ResetEnvironment
  final case class Action(value: Int)
}

class Reality(envID: String) extends Actor with ActorLogging {
  val environment: GymInstance = gymClient.execute(createEnv(envID))

  def receive: Receive = {
    case Observer(eyes) =>
      log.info("Welcome Summoner!")
      context.become(observed(eyes))
  }

  def observed(eyes: List[ActorRef]): Receive = {
    case ResetEnvironment =>
      val response: Observation = gymClient.execute(resetEnv(environment))

      for {
        x <- response.observation zip eyes
      } yield {
        x._2 ! Signal(x._1)
      }

    case Action(value: Int) =>
      val response: StepReply = gymClient.execute(step(environment, value, render = true))

      val inputs: List[Double] = response.observation

      for {
        x <- inputs zip eyes
      } yield {
        x._2 ! Signal(x._1)
      }
  }
}
