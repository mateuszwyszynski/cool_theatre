import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfter, Matchers, WordSpecLike}
import theatre.Brain.{CheckStemCells, Create, CreateStem}
import theatre.CPPN.StemCellReadyToUse
import theatre.Neurone.{EstablishConnection, LookForConnections, LookingForConnections, Signal}
import theatre.VectorTools.{Point, cubeInterior, norm, normalize, projectOnSegment, scalarProduct, sigmoidalFunction, subtract}
import theatre.{Brain, CPPN, CellType, CheckerCell, Genome, Neurone, NeuroneCell, OutputCell, StemCell, VectorTools}
import theatre.GenomeOperations.{createBaseNodes, createConnections, sortByReward}
import theatre.Genes.{ConnectionGene, NodeGene, OutputGene, SensorGene}

class neuroneStage() extends TestKit(ActorSystem("MySpec"))
  with Matchers
  with WordSpecLike
  with BeforeAndAfter {
  def sumList(list: List[Double]): Double = list.sum

  def deterministicCellType(
                         position: Point,
                         resources: Double,
                       ): CellType = {
    val randomizer = scala.util.Random
    val position: Point =
      (0.5, 0.5, 0.6)
    val axonCoordinates: Point =
      (randomizer.nextDouble(), randomizer.nextDouble(), randomizer.nextDouble())
    val rad = 0.4
    val threshold = randomizer.nextDouble()

    NeuroneCell(position, axonCoordinates, rad, sigmoidalFunction, threshold)
  }

  def randomNeuronesGenerator(position: Point, resources: Double): CellType = {
    val randomizer = scala.util.Random
    val pos: Point = (randomizer.nextDouble(), randomizer.nextDouble(), randomizer.nextDouble())
    val axCoor: Point = (randomizer.nextDouble(), randomizer.nextDouble(), randomizer.nextDouble())
    val rad = randomizer.nextDouble()
    val threshold = randomizer.nextDouble()

    NeuroneCell(pos, axCoor, rad, sigmoidalFunction, threshold)
  }

  val connections: List[ConnectionGene] =
    createConnections(4, 11)

  val baseNodes: Map[Int, NodeGene] = createBaseNodes(4, 11)

  val genome: Genome = Genome(baseNodes, connections)

  val zeroVec: Point = (0, 0, 0)
  val oneVec: Point = (1, 1, 1)

  "Base nodes" should {
    "consist of two inputs and one output" in {
      createBaseNodes(2,1).keys should equal (Set(1,2,3))
    }
    "consist of two inputs" in {
      createBaseNodes(
        2,
        1).
        filter(x => x._2.isInstanceOf[SensorGene]).keys should equal (Set(1,2))
    }
    "consist of one output" in {
      createBaseNodes(
        2,
        1).
        filter(x => x._2.isInstanceOf[OutputGene]).keys should equal (Set(3))
    }
  }

  "A middle neurone" should {
    "propagate signal which is bigger than threshold" in {
      val probe = TestProbe()

      val inputNeurone =
        system.actorOf(
          Neurone.props(zeroVec, oneVec, 0.5, sigmoidalFunction, 0),
          "Inputer1"
        )

      val middleNeurone =
        system.actorOf(
          Neurone.props(zeroVec, oneVec, 0.5, sigmoidalFunction, 0.5),
          "Middler1"
        )

      middleNeurone.tell(EstablishConnection(), probe.ref)

      inputNeurone.tell(EstablishConnection(), middleNeurone)

      inputNeurone ! Signal(1.0)

      val response = probe.expectMsgType[Neurone.Signal]

      response.value should be > 0.6
    }
    "not propagate when signal is lower than threshold" in {
      val probe = TestProbe()

      val inputNeurone =
        system.actorOf(
          Neurone.props(zeroVec, oneVec, 0.5, sigmoidalFunction, 0),
          "Inputer2"
        )

      val middleNeurone =
        system.actorOf(
          Neurone.props(zeroVec, oneVec, 0.5, sigmoidalFunction, 2),
          "Middler2"
        )

      middleNeurone.tell(EstablishConnection(), probe.ref)

      inputNeurone.tell(EstablishConnection(), middleNeurone)

      inputNeurone ! Signal(1)

      probe.expectNoMessage
    }
  }

  "The shouldFire function" should {
    "return true" in {
      Neurone.shouldFire(List(1,2), 0.5, sigmoidalFunction) should equal(true)
    }
    "return false" in {
      Neurone.shouldFire(List(1,2), 4, sigmoidalFunction) should equal(false)
    }
  }

  "A neurone" when {
    "THIS TESTS WILL HAVE TO BE REVISED WHEN CONNECTIONS WEIGHTS ARE IMPLEMENTED: " +
      "it has received a signal" should {
      "add new signal from yet unregistered neurone" in {
        val probe = TestProbe()

        val newState = Neurone.updateNeuroneState(3, probe.ref, List())

        newState should equal(List((probe.ref, 3)))
      }
      "update signal from known source" in {
        val probe = TestProbe()

        val newState = Neurone.updateNeuroneState(3, probe.ref, List((probe.ref, 2)))

        newState should equal(List((probe.ref, 3)))
      }
    }
    "it has received a message from a neurone looking for connections" should {
      "establish connection if an incoming axon ends inside the input area" in {
        val probe = TestProbe()

        val outputNeurone: ActorRef =
          system.actorOf(Neurone.props(zeroVec, oneVec, 2, sigmoidalFunction, 0))

        outputNeurone.tell(LookingForConnections((1.5, 1.5, 1.5), (-1.0, -1.0, -1.0)), probe.ref)

        probe.expectMsgType[EstablishConnection]
      }
      "establish connection if an incoming axon intersects the input area" in {
        val probe = TestProbe()

        val outputNeurone: ActorRef =
          system.actorOf(Neurone.props(zeroVec, oneVec, 1, sigmoidalFunction, 0))

        outputNeurone.tell(LookingForConnections((0.5, 0, 1.5), (0, 0, -3.0)), probe.ref)

        probe.expectMsgType[EstablishConnection]
      }
      "not establish connection if an incoming axon is outside the input area" in {
        val probe = TestProbe()

        val outputNeurone: ActorRef =
          system.actorOf(Neurone.props(zeroVec, oneVec, 1, sigmoidalFunction, 0))

        outputNeurone.tell(LookingForConnections((0.5, 0, 1.5), (0.5, 0, 2.5)), probe.ref)

        probe.expectNoMessage
      }
      "not establish connection if it has received a message from itself - " +
        "to check this you should read the log info and make sure that SelfGuardian actor" +
        "said he shouldn't connect to a neurone" in {
        val outputNeurone: ActorRef =
          system.actorOf(
            Neurone.props(zeroVec, oneVec, 1, sigmoidalFunction, 0),
            "SelfGuardian"
          )

        outputNeurone.tell(LookingForConnections(zeroVec, oneVec), outputNeurone)
      }
    }
  }

  "Base genome creation:" when {
    "function createBaseNodes is used it" should {
      "create 1 input node" in {
        val nodes: Map[Int, NodeGene] = createBaseNodes(1,1)

        nodes(1).isInstanceOf[SensorGene] should equal(true)
      }
      "create 1 output node" in {
        val nodes: Map[Int, NodeGene] = createBaseNodes(1,1)

        nodes(2).isInstanceOf[OutputGene] should equal(true)
      }
    }
  }

  "CPPN actor" should {
    "create a checker cell or neurone cell" in {
      val probe = TestProbe()

      val cppnActor = probe.childActorOf(CPPN.props(cubeInterior, genome), "CPPNActor")

      cppnActor.tell(StemCellReadyToUse((1.0, 1.0, 1.0), 1.0, "ProbeStemCell1"), probe.ref)

      val response = probe.expectMsgType[Create]

      response.cell.isInstanceOf[CheckerCell] || response.cell.isInstanceOf[NeuroneCell] should equal(true)
    }
    "create no neurone" in {
      val probe = TestProbe()

      val cppnActor = system.actorOf(CPPN.props(cubeInterior, genome), "CPPNActor")

      cppnActor.tell(StemCellReadyToUse((1.0, 1.0, 1.0), 0, "ProbeStemCell2"), probe.ref)

      probe.expectNoMessage
    }
  }

  "Stem Cell" should {
    "not create more neurones than it has resources" in {
      val brain = system.actorOf(Brain.props(genome, cubeInterior), "ResourceBrain")

      brain ! Create(StemCell((0.5, 0.5, 0.0), 0), "")
      brain ! Create(StemCell((0.25, 0.25, 0.25), 1), "")

      Thread.sleep(100)

      brain ! Create(NeuroneCell((0.2, 0.2, 0.2), (0.1, 0.1, 0.1), 0.15, sigmoidalFunction, 0.4), "StemCell1")
      brain ! Create(NeuroneCell((0.8, 0.8, 0.8), (0.1, 0.1, 0.1), 0.15, sigmoidalFunction, 0.4), "StemCell1")

      val probe = TestProbe()

      Thread.sleep(100)

      brain.tell(LookingForConnections((0.8, 0.8, 0.9), (0.0, 0.0, -0.05)), probe.ref)

      probe.expectNoMessage
    }
  }

  "General test - check the log" in {
    val probe = TestProbe()

    val brain = system.actorOf(Brain.props(genome, cubeInterior), "RandomBrain")

    brain ! CreateStem((0.5, 0.5, 0.0), 0, "Hands")

    brain ! Create(OutputCell((0.5, 0.5, -0.1), 0.6, probe.ref), "Hands")
    brain ! Create(OutputCell((0.25, 0.5, -0.2), 0.4, probe.ref), "Hands")
    brain ! Create(OutputCell((0.75, 0.5, -0.1), 0.4, probe.ref), "Hands")

    Thread.sleep(100)

    brain ! Create(StemCell((0.25, 0.25, 0.25), 100), "StemCell1")
    brain ! Create(StemCell((0.25, 0.25, 0.75), 100), "StemCell2")
    brain ! Create(StemCell((0.25, 0.75, 0.25), 100), "StemCell3")
    brain ! Create(StemCell((0.25, 0.75, 0.75), 100), "StemCell4")
    brain ! Create(StemCell((0.75, 0.25, 0.25), 100), "StemCell5")
    brain ! Create(StemCell((0.75, 0.25, 0.75), 100), "StemCell6")
    brain ! Create(StemCell((0.75, 0.75, 0.25), 100), "StemCell7")
    brain ! Create(StemCell((0.75, 0.75, 0.75), 100), "StemCell8")

    Thread.sleep(100)

    brain ! CheckStemCells()

    Thread.sleep(100)

    val inputNeurone =
      system.actorOf(
        Neurone.props((0.5, 0.5, 1.0), (-0.0, -0.0, -0.3), 0.5, sigmoidalFunction, 0),
        "Inputer"
      )

    brain.tell(LookingForConnections((0.5, 0.5, 1.0), (-1.0, -1.0, -1.0)), inputNeurone)

    Thread.sleep(100)

    inputNeurone ! Signal(2)

    Thread.sleep(100)

    brain ! LookForConnections()
  }

  "List of genomes with rewards List((genome,2.0), (genome, 3.0), (genome, 1.0))" should {
    "be equal to List((genome,3.0), (genome, 2.0), (genome, 1.0)) after sorting" in {
      val genomesUnsorted: List[(Genome, Double)] = List((genome,2.0), (genome, 3.0), (genome, 1.0))
      val genomesSorted: List[(Genome, Double)] = List((genome,3.0), (genome, 2.0), (genome, 1.0))

      sortByReward(genomesUnsorted) should equal (genomesSorted)
    }
  }

  "Projection on a segment" should {
    "be equal (0.5, 0.0, 0.0)" in {
      projectOnSegment((0.5, 0, 1.5), (0.5, 0, -1.5), zeroVec) should equal((0.5, 0.0, 0.0))
    }
    "be almost (0.0, 0.0, 0.0)" in {
      val projection = projectOnSegment((0.5, 0.5, 0.0), (-0.5, -0.5, 0.0), (0.0, 0.0, 3.0))
      val isCloseToZero = norm(subtract(projection, (0.0, 0.0, 0.0))) <= 0.00001
      isCloseToZero should equal(true)
    }
    "be equal (0.5, 0.0, 1.5)" in {
      val projection = projectOnSegment((0.5, 0.0, 1.5), (0.5, 0.0, 2.5), (0.0, 0.0, 0.0))
      projection should equal((0.5, 0.0, 1.5))
    }
  }

  "Normalized vector" should {
    "be equal (0.0, 0.0, -1.0)" in {
      val result: Point = (0.0, 0.0, -1.0)
      normalize(subtract((0.5, 0, -1.5), (0.5, 0, 1.5))) should equal(result)
    }
    "be equal (1/sqrt(2), 1/sqrt(2), 0)" in {
      val result: Point = (-1/Math.sqrt(2), -1/Math.sqrt(2), 0.0)
      normalize(subtract((-0.5, -0.5, 0.0), (0.5, 0.5, 0.0))) should equal(result)
    }
  }

  "Subtract" in {
    val result: Point = (0.0, 0.0, 3.0)
    subtract((0.5, 0, 1.5), (0.5, 0, -1.5)) should equal(result)
  }

  "length of projection" in {
    val normalizedDirection: Point = normalize(subtract((0.5, 0, -1.5), (0.5, 0, 1.5)))
    val length: Double = scalarProduct(normalizedDirection, subtract(zeroVec, (0.5, 0, 1.5)))
    length should equal(1.5)
  }

  "projecting on direction" in {
    val result: Point = (0.5, 0.0, 0.0)
    VectorTools.projectOnDirection((0.5, 0, 1.5), (0.5, 0, -1.5), zeroVec) should equal(result)
  }
}
