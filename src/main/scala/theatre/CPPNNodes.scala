package theatre

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import theatre.CPPNQuery.Result
import theatre.ParameterNode.Parameter
import theatre.SensorNode.{Input, AddOutputNode}

object SensorNode {
  def props(procFun: Double => Double): Props = Props(new SensorNode(procFun))

  final case class Input(value: Double)
  final case class AddOutputNode(nodeActor: ActorRef, weight: Double)
}

class SensorNode(procFun: Double => Double) extends Actor with ActorLogging {
  var outputNodesWeights: Map[ActorRef, Double] = Map.empty[ActorRef, Double]

  def receive: Receive = {
    case Input(v) =>
      for {
        x <- outputNodesWeights
      } yield {
        x._1 ! Input(procFun(v * x._2))
      }
    case AddOutputNode(n, w) =>
      outputNodesWeights += n -> w
  }
}

object HiddenNode {
  def props(procFun: Double => Double, threshold: Double): Props =
    Props(new HiddenNode(procFun, threshold))
}

class HiddenNode(procFun: Double => Double, threshold: Double) extends Actor with ActorLogging {
  var outputNodesWeights: Map[ActorRef, Double] = Map.empty[ActorRef, Double]

  var inputSignals: Map[ActorRef, Double] = Map.empty[ActorRef, Double]

  def receive: Receive = {
    case Input(v) =>
      inputSignals = inputSignals.updated(sender, v)
      val processedSignal: Double = procFun(inputSignals.values.sum)
      if(processedSignal >= threshold) {
        for {
          x <- outputNodesWeights
        } yield {
          x._1 ! Input(processedSignal * x._2)
        }
      } else {}
    case AddOutputNode(n, w) =>
      outputNodesWeights = outputNodesWeights.updated(n, w)
  }
}

object ParameterNode {
  def props(procFun: Double => Double, baseValue: Double): Props =
    Props(new ParameterNode(procFun, baseValue))

  final case class Parameter()
}

class ParameterNode(procFun: Double => Double, baseValue: Double) extends Actor with ActorLogging {
  var outputNodesWeights: Map[ActorRef, Double] = Map.empty[ActorRef, Double]

  def receive: Receive = {
    case Parameter() =>
      for {
        x <- outputNodesWeights
      } yield {
        x._1 ! Input(procFun(baseValue) * x._2)
      }
    case AddOutputNode(n, w) =>
      outputNodesWeights = outputNodesWeights.updated(n, w)
  }
}

object OutputNode {
  def props(procFun: Double => Double, threshold: Double): Props =
    Props(new OutputNode(procFun, threshold))
}

class OutputNode(procFun: Double => Double, threshold: Double) extends Actor with ActorLogging {
  var outputNodesWeights: Map[ActorRef, Double] = Map.empty[ActorRef, Double]

  var inputSignals: Map[ActorRef, Double] = Map.empty[ActorRef, Double]

  def receive: Receive = {
    case Input(v) =>
      inputSignals = inputSignals.updated(sender, v)
      val processedSignal: Double = procFun(inputSignals.values.sum)
      if(processedSignal >= threshold) {
        context.parent ! Result(processedSignal)
      } else {}
  }
}
