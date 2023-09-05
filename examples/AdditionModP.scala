package joygp.examples

import joygp.*
import Evolver.{ScoredIndividual,RandBetween}
import JoyBool.*
import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scodec.bits._

import spire.math._

trait AdditionModP:
  val thePrime: Int = 31

  case class TestCase(initialState: ProgramState, expectedFinalState: ProgramState)

  val example = TestCase(
    initialState = ProgramState().copy(input = BitVector.fromInt(18) ++ BitVector.fromInt(19)),
    expectedFinalState = ProgramState().copy(output = BitVector.fromInt((18 + 19) % thePrime))
  )
  
  def generateTestCases(n: Int): IO[List[TestCase]] = List.range(0,n).traverse{
    i => for {
      rand <- randomIO
      lhs <- rand.betweenInt(0,thePrime)
      rhs <- rand.betweenInt(0,thePrime)
      result = (lhs + rhs) % thePrime
      // encode the above inputs into the initial state input bitvector
      initialState <- IO(ProgramState().copy(input = BitVector.fromInt(lhs) ++ BitVector.fromInt(rhs)))
      expectedState <- IO(ProgramState().copy(output = BitVector.fromInt(result)))
    } yield TestCase(initialState,expectedState)
  }

  /** number of test cases to create for each individual assessment */
  val numTestCases: Int = 10

  val fitness: Program => IO[Double] = candidate => {
    /**
      * fitness should be linear if possible?
      * 
      * + 100 for each input bit read
      * + 100 for each correct output bit
      * + 10000 if output is correct length
      * - 1000 for each item left on exec stack
      * - 100 for each opcode (want shortest program)
      * repeat for each test case
      */
    generateTestCases(numTestCases).flatMap{
      tests => tests.parTraverse{
        test => candidate(test.initialState).flatMap{
          candidateFinalState =>
            IO(score(test.expectedFinalState,candidateFinalState))
        }
      }
    }.map(_.sum)
  }

  // where we actually calculate the "score"
  def score(expected: ProgramState, candidate: ProgramState): Double = {
    val execStackSizeDiff = math.abs(expected.exec.size - candidate.exec.size).toDouble
    val inputStackSizeDiff = math.abs(expected.input.size - candidate.input.size).toDouble
    val outputSizeDiff = math.abs(expected.output.size - candidate.output.size)
    val outputValueDiff = if(outputSizeDiff == 0)
                              UInt(expected.output.xor(candidate.output).toInt())
                          else
                              UInt.MaxValue - UInt(1)
    List(
      math.log(Int.MaxValue.toDouble - execStackSizeDiff),
      math.log(Int.MaxValue.toDouble - inputStackSizeDiff),
      math.log(Int.MaxValue.toDouble - outputSizeDiff),
      math.log((UInt.MaxValue - outputValueDiff).toDouble).ensuring(_ >= 0) // if output size not same, this component is zero
    ).sum
  }.ensuring(_ > 0)

  val maxPossibleScore:Double = score(example.expectedFinalState,example.expectedFinalState)*numTestCases

  val randomIO: IO[std.Random[IO]] = std.Random.scalaUtilRandom

  val bestYet = IO(
    //"33175f775b45d3f9e776c173ddee3ff67234c6dadb9428eec4d36cf43830ef3e26b72f1bbe461afedae115d3979fcb04a16676ae42866f36dbe2980bfea822fa1707fb5b2056a28d2d2782fcc2ada2036fab87965b96c84afc9b97d7bd291a86f76831fae8d6cef1019e4f3f45461a06d577ea3be888fd1937a51a7ff7f05797d3a64f8de28610e4681b08cda9bfc9b6ca195df69cd0dcd54dbf8de48aa7b4bf83372280863867a4e7db171e9fd73c8df7013b64590cc7640de4e26c09ae9a3d3e0e6b1bcd8a0355d03be53357e0e608ae9a353e2e78376fa88c678a751ffa8ee0fe5be647d316dbc97c152fdd6a5aaad4608"
    //"2417dff54941fbf9962b4d7f5ce73f867ab5cf9acf3c68ececd76cd9af6bbdedea8f4619fa4b60cd5dda9a6d988707b4a7e487a42f77dcc35fda5e2eba76e2b1089c78cbc1ca50edc25b6e33935dd534d3ceb0af2c2436dc502528e863f377cce35fdb5922ba6881e6ef9d56ba3eb04f70a5e5d241099cffead8018bdc9e79821efbb6093c85c17b476815bc664b7ae33bb50b68a5de89e1c596e5b296b706a5f5074eb964ff390b6ca1df89a0c70625ba92bd0627e5474e9964d37d54219a4d177fa6b288aec993da01a7fb1701796d3a7dfccee8612e6481f7cc9edbfc9f64a194de6bcf8d4d3e4fb84e498e6b03ff33fb2848e346582e28b9314bfd71813d6287b8b79a4bb641d03e6b12bab35930b86bd07c0f35a13a1bbd500c11e73d073e5838bcf68fe28f984b5b92cd669d217a3d7589fc58bc148c33f13d073c58b8b4feb9e08eff4cf555dc27ad0966b34998b40a9ad5fe6cce1e0e0afa9a871f6fd95a7f68d467f404eda166b31eb03c3ebf7da64143e28161889d7f3aeb3d5b297ba7ab79f6686a4b4bf5892b42e933fa2848e346582c3aba714bbdb1831faa96b4279a47b642d42a6b523ab77830ba6bd27a0f65213859bdd40411e6bd873e58b8b4fd5fc7a3c752da5fac535a17c99dd142431a32e061c5c38b5fed8c18fb5495a13cd6789db42cd663b61787d5efb4c82078d02c30122fe75d67b9edaf64f5773ecd1d4969feb02d685d267f44092c78cb0587570e297fb63063e552d684f35bc6e85a074d6847566f06170d7a4f43ecb4271333ba80823ec7a0f7cb17169fabfcf468aa4b4b758a6f42b933ba28486346580c38b8614bedb0801f6e96b62f92cf2642d03a6b52bab37a30b86bd27a0f2529187170e297fb63063edd2d6c4f359e4c95a074d6a57566f06070d7a4f41e4a4270337aa80823cc7a0e5cb17169fd3cd278b61481f25ab5f58bc985a35e93c179ad09d8cfeaa0308f31ea39f0c5c5a7f4dd047dc25ad0962334e90d51e92c4fe2c5e2a471afa9e879e2c587a7f74f0577fb6faa3ae12d644b359a2c85a064d6a5f36670717057c4d428f37ecad3fbc7a77ea0276d2b7598ed15e1f5fbedb3481f340b0c459bf9df59aa7b4bf93d4f43a3475a5a5fcbd31c3fe2f31cfd63ffd3e86f8a3e215d284b159a5c052076f4a6b162f0787ac7c4541c7160e253bbf7a73fed17953e70b6b42d9ac53642d03a6b53f83b7018303be27a1c7ca161e9fdf2d59f7075b687d3dc7e82f2baf5ee99853f9a8b46aa75f8ff34f13da4bc9ee7c37902a909e7a0eda697f474adf7017364b90cd764250f9bc8bb84b5812cd669f316a1d35a9fc38ba1c1c11f12d473c50b8b6fef1e08b7f6d7505dc25ad0d66b34d00f01e9ad6fe69cf0e2e0af29a871e2ac95e7f7cb661d004eda162a515b0bc0ead3da66803c78171a89d7d3ad8344f287f27abd8d768e8434bf87a6185f85b759fa64c5afb09f1439c20a50862b74b88240e9a84f62cfe0f0e10708ec3de24de583f7ef0e33fa6faa76e33d6858318b6cc5a074d6e7f1663443875dc3f9dfc9ee7b69f9374f863205425a5dc18b052fe8fb5ffa02f6f9f259aec8581f1e9fd73e8de6c233605a0deb6448c8c26419aa9e2d2e8fe95b8d8a511f7497b4359ac73642743a6a53ecbb71a87c39e15a097c3151f9f9f3711f70139785824c767412f862a3f3f3e6a23ee0272f0b0498ec85e175cbfc3b481f34439d748af9da49aa7b0fec3ddfceb25756537f697b0d2fe2fb1fee03b4c9f219aec8521f1e8bd77acd63c2f362780fc9648ce0c2e409ae9e2c3e0cecaeb6afb592e86b5faa543508a8e1c"
    "2417dff54941fbf9d62b4d7f5ce63f867ab5cf9acf3c68ececd76cd9af6bbdedea8f4619fa4b60cd5dda9a6d988607b4b7e487a42f67dcc35fda5a2eba76e2b1089c78cbc1ca50edc25b6e33935dd534d7ceb0af2c2436dc502528ea63f377cce35fdb5922ba6881e6ef9d56ba3eb04f70a5e5d249099cffead8018bdc9e79821fdb36093c85c15b476815bc664b7ae33bb50b68a5de89e1c596e5b296b706a5f5074eb964ff390b6ca1df89a0c70625ba92bd0767e5454e9964d37d54219a4d177fa6b288aec983da01a7fb170179ed327dfccee8612e6481f7cc9edbfc9f64a194de6bcf8d4d3e4fb84e498e6b03ff33fb2848e346582e28b9314bfd71813d6287b8b59a4bb641d03e6b12bab35930b86bd07c0f35a13a1bbd500c11e73d073e5838bcf68fe28f984b5bd2cd669d217a3d7589fd58bd148c33f13d073c58a8b4feb9e08eff4cf555dc27ad0966b34998b40a9ad5de6cce1e0e0afa9a871f6fd85a7f68d427f484eda166b31eb03c3eff7da64143e2816188997f3aeb3d5b097ba7ab79f6686a4b4bf5892b42e933fa2848eb46582c3abe714bbdb1831eaa96b4279a4fb642c42a6b523a377830ba6bd27a0f65213859bdd40411e6bd873e58b8b4fd5fc7a34752da5fac535a17c99dd142431b32e061c5c38b5eed8c18fb5495a13cd6789db42cd663b61587d5efb4c82078d02c30122fe75d67b9edaf64f5777edd1d4969feb02d285d267f44092478cb0587570e297fb63063e552d684f35bc6e85a074d6847566f06170d7a4f43ecb4271333ba80823ec7a0f7cb17149fabfcf468aa4b0b758a6f42b933ba28486346180c38b8614bedb0801f6e96b62f92cf2642d03a6b52bab17a30b86bd27a0f2529187170e297fb63063edd2d6c4f359e4c95a074d6a57566f06070d7a4f41e4a4270337aa80823cc7a0e58b17169fd3cd278b61481f25ab5f58bc985a35693c179ad09d8cdeaa0308f31ea39f0c5c5a7f4dd047dc65ad0962334e90d41e92c4fe2c5e2a471afa9e879e2c587a7f74f0577eb6bae3ae02d644b35932c85a064d6a5f36650717057c4d428f37ecad3fbc7a77ea0276d2b7598ed15e1f5fbedb3481f340b08459af9df59aa7b4bf93d4f43a3475a5a5fcbd31c3fe2f31cfd63ffd3e86f8a3e215d284b159a5c052076f5a6b162f0787ac7c4541c71606251bbf7a33fed17953e70b6b42d9ac53642d03aeb53f83b7018303be27a1c7ca161e9fdf2d59f3075b687d3dc6e82f2baf5ae99853d9a8b46aa75f8bf34f13da4bc9ee7c37902a909e7a0eda697e4742df7017364b90cd764250f9bc8bb84b5812cd669f316a1d35a9fc38ba1c1e11f12d473c50b8b6fef1e08b7f6d7505dc25ad0d66b34d00f01e9ad6fee9cf0e2e0a729a871e0ac95e7f7cb660d004eda162a515b0bc0ead3da66803c7a171a89d7d3ad8344f287f27abd8d768e8434bf87a6185f85b759fa64c5afb09f1439c20a50862b74b88240e9a84f62cfe0f0e10708ec3de24de583f7ef0e33fa6faa76e33d6858318b6cc5a074d6e7f1663443875dc3f9dfc9ee7b69f9374f863205425a5dc18b052fe8fb5ffa02f2f9f259aec8581f9e9fd73e8de6c233605a1deb6048c8ca6419aa9e2d2e8fe95b8d8a511f7497b6359ac73642743b6a53ecbb71e87c39e17a097c3151f1f9f1711f70139785824c76741af862a3f3f3a6a23ee0272f0b0498ecc5e075cbfc3b481f24439d748af9da49aa7b07ec3ddfceb25752537f69fb0d2fe0fb1fee03b4c9f219aec8521f1e8bd77acd63c2f362780fc9649c60c2e409ae9e2c3e0cecaeb6afb592e86b5faa543508a8e1c"  
  ).map(hex => BitVector.fromValidHex(hex)).flatMap(bits => fitness(Program.parse(bits)).map(score => ScoredIndividual(bits,score)))
  
  val startingPop = bestYet.map(best => List.fill(100)(best))
  /*val startingPop: IO[List[ScoredIndividual[Double]]] = List.range(0,100).parTraverse{ i => 
    for {
      size <- randomIO.flatMap(_.betweenInt(5,100))
      bits <- randomIO.flatMap(_.nextBytes(size)).map(BitVector(_))
      indiv <- IO(Program.parse(bits))
      score <- fitness(indiv)
    } yield ScoredIndividual(bits,score)
  }*/

  // evolve n generations
  def evolve(n: Int, numParallel: Int, printEvery: Int = 10, maxTarget: Option[Double] = None)
              (implicit ord: cats.kernel.Order[Double], ring: spire.algebra.Ring[Double], randbetween: RandBetween[Double])
              :IO[List[ScoredIndividual[Double]]] = for { 
      rand <- randomIO
      pop <- startingPop
      evolvedPop <- Evolver.evolveN(fitness)(pop,n,numParallel, printEvery,maxTarget)(Program.geneticJoyBool,rand,ord,ring,randbetween)
  } yield evolvedPop

  def test(candidate: Program, numTests:Int = numTestCases): IO[Unit] = for {
    cases <- generateTestCases(numTests)
    output <- cases.traverse(testcase => for {
        _ <- IO.println("-----")
        _ <- IO.println(testcase.initialState)
        _ <- IO.println(testcase.expectedFinalState)
        output <- candidate(testcase.initialState)
        _ <- IO.println(output)
        _ <- IO.println("-----")
      } yield ()      
    )

  } yield ()

  def hamming(lhs: Int, rhs: Int): Int = java.lang.Integer.bitCount(lhs ^ rhs)
  extension(i: Int)
    def bitCount: Int = java.lang.Integer.bitCount(i)