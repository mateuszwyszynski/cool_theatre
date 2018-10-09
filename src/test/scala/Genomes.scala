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
}