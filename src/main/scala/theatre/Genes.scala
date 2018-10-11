package theatre

object Genes {
  trait NodeGene {}

  object SensorGene {
    def apply(): NodeGene = new SensorGene()
  }

  object OutputGene {
    def apply(baseValue: Double): NodeGene = new OutputGene(baseValue)
  }

  object HiddenGene {
    def apply(baseValue: Double): NodeGene = new HiddenGene(baseValue)
  }

  object ParameterGene {
    def apply(baseValue: Double): NodeGene = new ParameterGene(baseValue)
  }

  case class SensorGene() extends NodeGene {}
  case class OutputGene(baseValue: Double) extends NodeGene {}
  case class HiddenGene(baseValue: Double) extends NodeGene {}
  case class ParameterGene(baseValue: Double) extends NodeGene {}

  object ConnectionGene {
    def apply(
               input: Int,
               output: Int,
               weight: Double,
               enabled: Boolean
             ): ConnectionGene = new ConnectionGene(input, output, weight, enabled)
  }

  case class ConnectionGene(
                             input: Int,
                             output: Int,
                             weight: Double,
                             enabled: Boolean
                           ) {
    def enableDisable(): ConnectionGene = {this.copy(enabled = !enabled)}

    def disableIfLike(connection: ConnectionGene): ConnectionGene = {
      if(this == connection) {
        this.enableDisable()
      } else this
    }
  }

  final case class WrongConnectionGene(private val msg: String = "", private val cause: Throwable = None.orNull)
    extends Exception(msg, cause)
}
