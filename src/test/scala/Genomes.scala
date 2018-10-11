import org.scalatest.{BeforeAndAfter, Matchers, WordSpecLike}
import theatre.VectorTools._
import theatre._
import Genes._

class Genomes extends Matchers
  with WordSpecLike
  with BeforeAndAfter {
  val connections: List[ConnectionGene] =
    createConnections(4, 11)

  val baseNodes: Map[Int, NodeGene] = createBaseNodes(4, 11)

  val genome: Genome = Genome(baseNodes, connections)

  "Crossing over two identical genomes" should {
    "result in the same genome" in {
      genome.mateWith(genome) should equal(genome)
    }
  }

  "Crossing over genome with 4 connections with genome with 2 connections" when {
    "crossing is after the end of longer genome" should {
      "yield the longer genome" in {
        val connections1: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true))

        val connections2: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes1: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)))

        val nodes2: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val genome1: Genome = Genome(nodes1, connections1)

        val genome2: Genome = Genome(nodes2, connections2)

        val child: Genome =
          crossOverGenomesAt(genome1, genome2, genome1.connections().length + genome2.connections().length)

        child should equal(genome2)
      }
    }

    "crossing is after 3rd connection of the longer genome" should {
      "yield genome containing just the first three connections of the longer genome" in {
        val connections1: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true))

        val connections2: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes1: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)))

        val nodes2: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val genome1: Genome = Genome(nodes1, connections1)

        val genome2: Genome = Genome(nodes2, connections2)

        val child: Genome =
          crossOverGenomesAt(genome1, genome2, genome1.connections().length + genome2.connections().length - 1)

        val resultConns: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true))

        val resultNodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val resultGenome: Genome = Genome(resultNodes, resultConns)

        child should equal(resultGenome)
      }
    }

    "crossing is after the end of the shorter genome" should {
      "yield genome containing the first two connections of the shorter genome" +
        "and the last two connections of the longer genome" in {
        val connections1: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true))

        val connections2: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes1: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)))

        val nodes2: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val genome1: Genome = Genome(nodes1, connections1)

        val genome2: Genome = Genome(nodes2, connections2)

        val child: Genome =
          crossOverGenomesAt(genome1, genome2, genome1.connections().length)

        val resultConns: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val resultNodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val resultGenome: Genome = Genome(resultNodes, resultConns)

        child should equal(resultGenome)
      }
    }

    "crossing in two different orders" should {
      "yield the same result" in {
        val connections1: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true))

        val connections2: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes1: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)))

        val nodes2: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val genome1: Genome = Genome(nodes1, connections1)

        val genome2: Genome = Genome(nodes2, connections2)

        val child1: Genome =
          crossOverGenomesAt(genome1, genome2, genome1.connections().length + genome2.connections().length - 1)

        val child2: Genome =
          crossOverGenomesAt(genome2, genome1, genome2.connections().length - 1)

        child1 should equal(child2)
      }
    }

    "crossing is after 2nd connection of the longer genome" should {
      "yield genome with just the first two connections of the longer genome" in {
        val connections1: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2,3,1.0, enabled = true))

        val connections2: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes1: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)))

        val nodes2: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()),
            (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val genome1: Genome = Genome(nodes1, connections1)

        val genome2: Genome = Genome(nodes2, connections2)

        val child: Genome =
          crossOverGenomesAt(genome1, genome2, genome1.connections().length + genome2.connections().length - 2)

        val resultConns: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false))

        val resultNodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (3, OutputGene(0.7)))

        val resultGenome: Genome = Genome(resultNodes, resultConns)

        child should equal(resultGenome)
      }
    }
  }

  "Adding new node gene to a genome" should {
    "yield the same genome" when {
      "there is no such connection in genome" in {
        val connections: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true))

        val nodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.6)), (4, HiddenGene(0.7)))

        val genome: Genome = Genome(nodes, connections)

        genome.addNodeAtConnection(4, 3, 0.7) should equal(genome)
      }

      "specified connection is disabled" in {
        val connections: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true))

        val nodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.7)))

        val genome: Genome = Genome(nodes, connections)

        genome.addNodeAtConnection(2, 3, 0.5) should equal(genome)
      }
    }

    "yield a genome with new node at specified connection" in {
      val connections: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 0.7, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true))

      val nodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)))

      val genome: Genome = Genome(nodes, connections)

      val resultConns: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 0.7, enabled = false), ConnectionGene(2, 3, 1.0, enabled = true),
          ConnectionGene(1, 4, 1.0, enabled = true), ConnectionGene(4, 3, 0.7, enabled = true))

      val resultNodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val resultGenome: Genome = Genome(resultNodes, resultConns)

      genome.addNodeAtConnection(1, 3, 0.4) should equal(resultGenome)
    }
  }

  "Adding a new connection gene to a genome" should {
    "add new connection" in {
      val connections: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

      val nodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val genome: Genome = Genome(nodes, connections)

      val resultConns: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(1, 4, 0.5, enabled = true))

      val resultNodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val resultGenome: Genome = Genome(resultNodes, resultConns)

      genome.addConnectionBetween(1, 4, 0.5) should equal(resultGenome)
    }

    "enable and change weight of a disabled connection with the same input and output" in {
      val connections: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

      val nodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val genome: Genome = Genome(nodes, connections)

      val resultConns: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 0.5, enabled = true),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

      val resultNodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val resultGenome: Genome = Genome(resultNodes, resultConns)

      genome.addConnectionBetween(2, 3, 0.5) should equal(resultGenome)
    }

    "enable and change weight of only the first disabled connection with the same input and output" in {
      val connections: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(2, 3, 0.7, enabled = false))

      val nodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val genome: Genome = Genome(nodes, connections)

      val resultConns: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 0.5, enabled = true),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(2, 3, 0.7, enabled = false))

      val resultNodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val resultGenome: Genome = Genome(resultNodes, resultConns)

      genome.addConnectionBetween(2, 3, 0.5) should equal(resultGenome)
    }

    "enable and change weight of the first disabled connection with the same input and output and" +
      "do not change any enabled connections with the same input and output" +
      "which are farther in the genome then the disabled connection" in {
      val connections: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(2, 3, 0.7, enabled = true), ConnectionGene(2, 3, 0.6, enabled = true))

      val nodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val genome: Genome = Genome(nodes, connections)

      val resultConns: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 0.5, enabled = true),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(2, 3, 0.7, enabled = true), ConnectionGene(2, 3, 0.6, enabled = true))

      val resultNodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val resultGenome: Genome = Genome(resultNodes, resultConns)

      genome.addConnectionBetween(2, 3, 0.5) should equal(resultGenome)
    }

    "enable and change weight of the first disabled connection with the same input and output and" +
      "do not change any enabled connections with the same input and output" +
      "which are placed before the disabled connection in the genome" in {
      val connections: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(2, 3, 0.7, enabled = true), ConnectionGene(2, 3, 0.6, enabled = false))

      val nodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val genome: Genome = Genome(nodes, connections)

      val resultConns: List[ConnectionGene] =
        List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = true),
          ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true),
          ConnectionGene(2, 3, 0.7, enabled = true), ConnectionGene(2, 3, 0.5, enabled = true))

      val resultNodes: Map[Int, NodeGene] =
        Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

      val resultGenome: Genome = Genome(resultNodes, resultConns)

      genome.addConnectionBetween(2, 3, 0.5) should equal(resultGenome)
    }

    "throw a WrongConnectionGene exception" when {
      "specified input is wrong" in {
        val connections: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

        val genome: Genome = Genome(nodes, connections)

        val thrown =
          the [WrongConnectionGene] thrownBy genome.addConnectionBetween(5, 3, 0.5)

        thrown.getMessage should equal("No node found within the genome for the specified input.")
      }

      "specified output is wrong" in {
        val connections: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

        val genome: Genome = Genome(nodes, connections)

        val thrown =
          the [WrongConnectionGene] thrownBy genome.addConnectionBetween(2, 8, 0.5)

        thrown.getMessage should equal("No node found within the genome for the specified output.")
      }

      "both specified input and output are wrong" in {
        val connections: List[ConnectionGene] =
          List(ConnectionGene(1, 3, 1.0, enabled = true), ConnectionGene(2, 3, 1.0, enabled = false),
            ConnectionGene(2, 4, 1.0, enabled = true), ConnectionGene(4, 3, 1.0, enabled = true))

        val nodes: Map[Int, NodeGene] =
          Map((1, SensorGene()), (2, SensorGene()), (3, OutputGene(0.7)), (4, HiddenGene(0.4)))

        val genome: Genome = Genome(nodes, connections)

        val thrown =
          the [WrongConnectionGene] thrownBy genome.addConnectionBetween(5, 8, 0.5)

        thrown.getMessage should equal("No nodes found within the genome for both input and output.")
      }
    }
  }
}
