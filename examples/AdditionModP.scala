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
    "a417dff54941f9d9d62b4d7f5ce63f8672b5cf9acf3c68ccecd76cd9af6bbdedea8f4639fa4b60cd5dda9a7d988647b5b7e407a42f275cc35dda5a2eba77e2b3089c70cbc1ca50edc25bee3b935dd534c74eb0af282436bc502528ea67f377cce35fdb5922ba6c81e4ef9d56ba1eb04f7025a5d2c9099cffead9018bdc9e7b821fdb360d3d85c15b476815bc66497ae33bf50f68a5de89e1c196e5f2d6b706a5f5074eb964fd390b6ca1df89a0c706a6ba92bd0767e5454e9164537d50219a4d177fa6b28aaec983da00a5fb970179ed327deccee8612c6481f7cc9edbfc9f65a194de4bc70d4d3e4fb84e498e6b0bff33fa284ce346582e28b9314afd71813d6283b8b59a4ab641d03e6b129ab35930b84bd07c2f35e13a1bbd500c31c73d07365838bcf68fe28f98eb1bd2cd669d217a3d7589fd78bd148c33d13d073c58a8b4feb9a09eff4ef155dc27ad0966b34898f44a9a51de6ccf1e0e0afa8a871f6ed85a7f68d425f484eda166b31eb03c3e3f79ae4143e2916188897f3aeb3d53097327ab79f6686a4a4bd5a32b42f933fa2848eb46582c3afe714bbd31831eaab6b4279a6bb602c4aa6b52ba277830ba6bd27a0f752038593dd40411e6bd873e78b8b4fd5bc7a30742da5fac535a1fc99dd042431b32c0e1c5c38b5eed8c18fb5597a13cd6789cb42cd663b61587c5ffb4c82078d12c30122fe7d567b9edaf64f5757edc1d4869feb02d285d247f440b2578cb0487570e297fb63063e552d684f35bc7e85a074d6847566f06170d7a4f43ecb42313339a80823ec7a0f7ca17149f2bfcf468aa4b0f658a6f42b931ba28480346180c38b8614be5b0801f6e86b60fb2cf2642d03a6b50bab11e3098ebd27a0f2529187170e297fb62063edd2d6c4f379e4c95a074dee57566f06070d7a4e41e4a4270337aa80823cc7a0e58b17169fd38d278b61480f25ab5f58bc9a5a35693c179ad09d8cdeaa0308f31ea31f0c5c5a7d4dd0479c65ad0162334e10d40eb2c4fe2c5e2a471afa9e879a2c586a7f74f1575eb6bae7ae03d644b35932c85a064d6a5f36650737057c4d428f37ec2d3facfa75ea0276d2b769aed15e1f5fbedb3c81f340b08459af9df59aa7b4bf9354f41a7075a5a5fcbd31c3de3f31dfd63ffd3e86f8a3e215d284b158b5c052076d5a6b16070797ac7c4541c51606251bbf7a33fed17953e60b6b42d9ac43643d038eb53f93b7018303fe03a1c7ca161e97dd255bf1075b687f3dc6c82f2baf5ae99853d9a8b468a75f8bf34f13da5bc9ee7c37906a909e6a0eda697e4742df7017364b90cd764210f9bd8bb84b5812cd669b317a1d35b9fc38ba1c1e11f12d473c50b8b6fef5c08f7f6d7505dc25bd0d66b34d00f11c9ad7fee9cf0e2e1a729a871e0ac95e7d7cb6e4d004ed2162a515b8bc0ead3dae6803c7a1f0a895752ac8344f287f27abd8d768e8434bf85a6185f95b759fa64c5afb09f1539c20a50b62b74308200e9a84f62cfe0f061070cec3de24de583f7ef0e33f86faa72e33d6858318b6c85a074d6e5f16634438f5dc3f9dfc9ae7b69e9374f863005425a5dc18b052fe8fb5ffa02f2f9e259aec8581f9e9fd73e8de6c233645a1deb5048c8ca6411aa9e2d2e8ff95b8d8a531f7497b6359ac73642743b6a53ecbf71e85439c17a097c3151f1f9f1711771139787924c76341af866abf3fba6a23ee0272f8b0499ecc5e475cbfc3b483f24429d748af9de4baa1b07ec3ddfeeb21752537f69fb0d2fc0fb1fee03b4c97219aec8721f1e8bd7fbcd63c2f362780fc9649c60c2e409ae9e2c3e0ce4aeb6a7b592e86b5faa559508a8e1d"
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
  val refTheBest = Ref.of[IO,ScoredIndividual[Double]](ScoredIndividual(BitVector.empty,0.0))
  val tolerance = math.pow(10,-14)
  def onBest(si: ScoredIndividual[Double]): IO[Unit] = for {
    ref <- refTheBest
    current <- ref.get
    _ <- if((si.score - maxPossibleScore).abs <= tolerance) 
            ref.set(si) 
              >> IO.println(s"!!!!!!!!!!!!! TARGET ACHIEVED   !!!!!! $si") 
                >> IO.raiseError(new RuntimeException("TARGET ACHIEVED!!!"))
          else 
            IO.unit
  } yield ()

  // evolve n generations
  def evolve( n: Int, 
              numParallel: Int, 
              printEvery: Int = 10, 
              maxTarget: Option[Double] = None,
              updateBest: ScoredIndividual[Double] => IO[Unit] = si => IO.unit)
              (implicit ord: cats.kernel.Order[Double], ring: spire.algebra.Ring[Double], randbetween: RandBetween[Double])
              :IO[List[ScoredIndividual[Double]]] = for { 
      rand <- randomIO
      pop <- startingPop
      evolvedPop <- Evolver.evolveN(fitness)(pop,n,numParallel, printEvery,maxTarget,updateBest)(Program.geneticJoyBool,rand,ord,ring,randbetween)
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