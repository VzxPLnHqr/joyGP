package joygp.examples

import joygp.*
import Evolver.*
import JoyBool.*
import cats.effect._
import cats.effect.syntax.all._
//import cats.implicits._
import cats.syntax.all._

import scodec.bits._

import spire.math._
import spire.implicits._

object AdditionModP extends IOApp.Simple:
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
    //"a417dff54941f9d9d62b4d7f5ce63f8672b5cf9acf3c68ccecd76cd9af6bbdedea8f4639fa4b60cd5dda9a7d988647b5b7e407a42f275cc35dda5a2eba77e2b3089c70cbc1ca50edc25bee3b935dd534c74eb0af282436bc502528ea67f377cce35fdb5922ba6c81e4ef9d56ba1eb04f7025a5d2c9099cffead9018bdc9e7b821fdb360d3d85c15b476815bc66497ae33bf50f68a5de89e1c196e5f2d6b706a5f5074eb964fd390b6ca1df89a0c706a6ba92bd0767e5454e9164537d50219a4d177fa6b28aaec983da00a5fb970179ed327deccee8612c6481f7cc9edbfc9f65a194de4bc70d4d3e4fb84e498e6b0bff33fa284ce346582e28b9314afd71813d6283b8b59a4ab641d03e6b129ab35930b84bd07c2f35e13a1bbd500c31c73d07365838bcf68fe28f98eb1bd2cd669d217a3d7589fd78bd148c33d13d073c58a8b4feb9a09eff4ef155dc27ad0966b34898f44a9a51de6ccf1e0e0afa8a871f6ed85a7f68d425f484eda166b31eb03c3e3f79ae4143e2916188897f3aeb3d53097327ab79f6686a4a4bd5a32b42f933fa2848eb46582c3afe714bbd31831eaab6b4279a6bb602c4aa6b52ba277830ba6bd27a0f752038593dd40411e6bd873e78b8b4fd5bc7a30742da5fac535a1fc99dd042431b32c0e1c5c38b5eed8c18fb5597a13cd6789cb42cd663b61587c5ffb4c82078d12c30122fe7d567b9edaf64f5757edc1d4869feb02d285d247f440b2578cb0487570e297fb63063e552d684f35bc7e85a074d6847566f06170d7a4f43ecb42313339a80823ec7a0f7ca17149f2bfcf468aa4b0f658a6f42b931ba28480346180c38b8614be5b0801f6e86b60fb2cf2642d03a6b50bab11e3098ebd27a0f2529187170e297fb62063edd2d6c4f379e4c95a074dee57566f06070d7a4e41e4a4270337aa80823cc7a0e58b17169fd38d278b61480f25ab5f58bc9a5a35693c179ad09d8cdeaa0308f31ea31f0c5c5a7d4dd0479c65ad0162334e10d40eb2c4fe2c5e2a471afa9e879a2c586a7f74f1575eb6bae7ae03d644b35932c85a064d6a5f36650737057c4d428f37ec2d3facfa75ea0276d2b769aed15e1f5fbedb3c81f340b08459af9df59aa7b4bf9354f41a7075a5a5fcbd31c3de3f31dfd63ffd3e86f8a3e215d284b158b5c052076d5a6b16070797ac7c4541c51606251bbf7a33fed17953e60b6b42d9ac43643d038eb53f93b7018303fe03a1c7ca161e97dd255bf1075b687f3dc6c82f2baf5ae99853d9a8b468a75f8bf34f13da5bc9ee7c37906a909e6a0eda697e4742df7017364b90cd764210f9bd8bb84b5812cd669b317a1d35b9fc38ba1c1e11f12d473c50b8b6fef5c08f7f6d7505dc25bd0d66b34d00f11c9ad7fee9cf0e2e1a729a871e0ac95e7d7cb6e4d004ed2162a515b8bc0ead3dae6803c7a1f0a895752ac8344f287f27abd8d768e8434bf85a6185f95b759fa64c5afb09f1539c20a50b62b74308200e9a84f62cfe0f061070cec3de24de583f7ef0e33f86faa72e33d6858318b6c85a074d6e5f16634438f5dc3f9dfc9ae7b69e9374f863005425a5dc18b052fe8fb5ffa02f2f9e259aec8581f9e9fd73e8de6c233645a1deb5048c8ca6411aa9e2d2e8ff95b8d8a531f7497b6359ac73642743b6a53ecbf71e85439c17a097c3151f1f9f1711771139787924c76341af866abf3fba6a23ee0272f8b0499ecc5e475cbfc3b483f24429d748af9de4baa1b07ec3ddfeeb21752537f69fb0d2fc0fb1fee03b4c97219aec8721f1e8bd7fbcd63c2f362780fc9649c60c2e409ae9e2c3e0ce4aeb6a7b592e86b5faa559508a8e1d"
    //"a417dfe5e941f9f9d62b4d7f5c763f0472b5cf9acf3c68ccecd76cd9af6bbdedea8f4638fe4b60cd5dda9a7d988647b5b7e407a42f275cc35dda5a2eba7762f3089c70cbc1ca50ed825bee3b935ddf36474eb0af280436bc542520ea67f3774ce35fdb4922ba6481e4cfbd563a9ab04ff225a5d2c9099cffead901cbfc9e7b821fdb360d3d85c15b47e81dbc66497a633be50f68a5da89e1c196e5f3d23306a57943da2976a2787065b97cb5adc1a97d41d3a6593f4e429b2877e06871c1a8aea4af41d9f95153a45914df5604669345dfe9aca6afb260f680297ef5c45f5b4c9f7b333a184b19307d7323b6ff679968653792f1c3534f93ee139262dac2ffccfe8a1338d1960b8a2e4c73bf5c604e58a0ef2d6692bd90740e9ac4a6acd64c2e12f01d0bcd784e96ef54030c71cf41c5960a2f1da2f0a3663ac6f4bb59a7485e8f5d627f5e2f45230cf44f41cb962a2d3faf6827bbd3bc5577096b4259acd2263d32a694779b33c78380bea291c7dab6169fda35097d213b6859acc7ac070b87de6b9050f8a4586222dfcebac754c25cc9eade7d9a1a9292f568cad0be44fe8a123ad1960b0ebf9c52ef6e60c7aaadad09e69aeda0b12a9ad6ae89da0c2e9af49e83dd484e164f75010479af61cfbe2e2d3e1670e8c1d0b697cb14d287f267741090c2ccb5387170e2d7bf63063ed565e84f359ea7290b3599ed8561f17fed32081e344b0c048bf9f559ee7b6bd93d5d5fb707521a7fac0b4a17491fd302c95e3ac100d5c38a5eed0c08fd54b5a13cd6f1fa9681d31a11d59bc187c37e93d0f32d40c44ce6a0208fb1ec3dd285c527caff3d1a2892c1d9668bd0ae4c6e8a1200d186030e2e1850f96c2047dba1ad83eca3c990b50e9ad42eac4f8c263af49e83e94a461c5c38b5f6d889cfb74b5b17cde79305681d37b95d59bc181c35e9390793929c0cdeaa0208f21e83862c1c187f4e349e2d85213cb62d7d22f26960d4a4f05e6b8076337aa80c22cc7a8cfc317149f537411e7596b40588cd384350bacb12f8b178a91c6bea7a1e68b141a9fdd1c55dfadaebbeb8075902cde0e321600935a954d9901ed415f1254a3ddf98b4feb3edd7a809cf4adca6b941787d7efb6df207cd02c21166be76d67e9ed2fe4453d029c1d69697f2f4c70f78bccf7f58fff4fa1be28f88574a12c562d7014a1db569bd5c1c1e5ef1f11507145898946efde8cfbb45e54f9a2dad0b66b10d10f50ebad4fe4edc060c0f982e871f28587a5f74856fc51d6da1fcf71b20bccebd6ba6614f66a2d1a29d7e2fcd3c4f6b6f27b9f0de41aa4279a03b6905f91c0b7dc05cd92e4335f90843e6f62ee12d604b319a6cc5e874d267f0e2a8707843d4b51cf142e2dbfbf5021dfdb5d4177096f4359acd3403c4766b5efba73c38b869ca6a1c782b6579f5f2da934013b4858a9456e3f03ab4f6b9a00f1e87c2a35fb45f54f982dad0b66911590f40ebad4fe4acc0e0c0ff90ec71f28585a5f70956fc0156d81fcf71b20baaebd6ba6614f66a251a29f7e27cd3c4f696f27bbf4de01aa4a79a83b69a5f91d0b7dc05cd92e4335f90843e6f62ee72d604b359a6cc5e874d6e7f0e2e8707947c4b51cf14282dbfbd7023dfdb5f4177096f4359acd3403c4706b5ffba73c10b809ca6a1d782b2579f5f2d3934013b4858a9456e2f03bb4f6b9a00f1e87c2a255d0ab20d13ca1fc9eafe359a3a10d2fe1698617e56dd67e9131ebec27c54e70200e9a84f62cdf0f061070cec3de24de583f7ef0e33f86faa72e11d6858318b6c85a074d665f16634c38f5dc3f95fc1ae7b69e9374f863005425255c189052fe8bb5ffa02f2f9a279aee8588d9e9fd73ecde6c233645a1deb5008c8ca6411aa9e2d268ff95b9d8a531f7497b6359ac33642743b4a53ccbf71e85439c1da093c3153f1f8f1711771139f879a4c76241af866abf27ba6a23ee0272f8b0499ecc5e475cefc3b483f24429d748af9fe4baa1b07ec3ddfeeb21752537f29fb0d6fc0fb1fee0bb4c97a19acc8721b1e8bb7fbdd61c2f362780bc9649c60c2e409aebe2c3c0ce4aeb6a7b492e8695faa559508a8e1d"
    "2f3584af0fab9ef04497de6ee5fa3b50b0255719b38824d9e0f28355c6bef01684b967d789d8b1e8d0616b9d107db4eadc371063e20af64a31b4d025e3929fd9ff5b5bb1242e1c8c201dbc94cb99c97ab207cbc97dedb40957431784672ec30b26644f5b592d7fad78e1f719d7bbfe5045acc827b701fff6efacdd1167921efd22ff376cd060f0bc48b6ed064284e0fb729fed3ad3ddf1067121ffe32fd1664d17302bea1a8ec791686a4fe166b3aba4fd51cf9caa715ba9f48276c7bf7b75febb53fd95716fa1758ea10afce9ef62bb834c7d32965f8793835b754d3b6738f7d8868e79e8eade43bbfa807dcb5cb79d46f3a53f7edda6bbb20860419fe2731338d78ed71047cd3f0e721f5fd48b6afa6af741bac750a5bcf8f9b15f49a73f194b3b40cbc3adfa271d338d7a6c40053cd0706f21f9fc48b0ed2e53caa3fad09fff2ed35ef8543861efd139c9bca797738cb9a28f83290fafeac5b52c329ee50f9f86f7f9779aefc4a9070f409d77389a53f994b3f42cbc3adfa2715328cfa6c40053cd1f06f21fdfd48b4ef2e53caa3fad09fbf2ed35ef8543861efd13949bc2757738cb9a60f83290fafeac5b52c33dec50f9e86bff9779aefc4a9670f429d773f9af8388a3f58dc597a9e0e8d6dd731e99c239f620838e78783390eefea05f761729fe51edd8cfde8749aeec8a1f30f7e89cc0ce73eab34404f34fcb9c8793f172dab69aacd42eb19431733f3eec57d269c7e2d24fc0fa726b7e89c748e21fbf10014f343c19c85f7f522d3bcb94e2b8fef423ef8bb4d7fe150e187bf44e62671975dca20868a3e0cec7eb7ab1655b0ef7b942e7a12f7e59e6bbf5aa50479827d5eee6bf0e6a5f323719a733fab599c6cde64149be129f335af1135dfa72af3592f623f16225733521e959c97f7f522fbb4b94f338bfd423efcbb4d7ba552df337270135dea75af1592b623f17025732563e959c87f7f522dbb4bd4f7a8bef423ef0b6597fe452e1a6bf44e626799f59c820a39a765ca47eafaf16d5b0cf6b953e6a16ffe5566bbf50870c3dca75dece6bece229fc637192423f2b1c940cde74349be1ae67b43d2b390fefea45f769729e6717fa847df9769af70aa5be62e5e026bbd6eb526b256c47e2e042e6cac7d2b390fefee45b76b7abee517de847de16eb2ffc9a5cb4f7e89cc4cf33e1390414734ecb9c87c5f5e2deb419af72a1cd425ffcaacd77ea10e17d5f5e2dab419ef72a34d42dffcaacd77ea10e187b94ebb9dcd7c9c453fac6e324847e56392819bce86937c213a67b7e026bf94e15f2b21e817e0e05fe66add735af0135dea70ae1592f60bf0302dfb3561ed69e8178f0e7220519e55882214f7744bae444934fab7355bf2ed6f664a8bd0f2d24b5e8b4444e03c2b10d469e4ac9fdea4a3b8e13e4affc0b2785477bf3b64f957f1b1ab82f984baf4b4b0bb9fb2d11138cf6ac4193a3b2b27c7ab24d8404b92ade90cae551da7ce997b55fc2efee4b8cac9e1efc937145b2ca7fc030fb5678cf3647cc57b7b37b9227953cc7120cfa09cbd7d7ce98d"
  ).map(hex => BitVector.fromValidHex(hex)).flatMap(bits => fitness(Program.parse(bits)).map(score => ScoredIndividual(bits,score)))
  
  //val startingPop = bestYet.map(best => List.fill(100)(best))
  val startingPop: IO[List[ScoredIndividual[Double]]] = List.range(0,100).parTraverse{ i => 
    for {
      size <- randomIO.flatMap(_.betweenInt(5,100))
      bits <- randomIO.flatMap(_.nextBytes(size)).map(BitVector(_))
      indiv <- IO(Program.parse(bits))
      score <- fitness(indiv)
    } yield ScoredIndividual(bits,score)
  }
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

  def fx(candidate: Program): Int => Int => IO[Int] = lhs => rhs => {
    val psi = ProgramState().copy(input = BitVector.fromInt(lhs) ++ BitVector.fromInt(rhs))
    candidate(psi).map(_.output.toInt())
  }

  //override protected def blockedThreadDetectionEnabled = true
  def run: IO[Unit] = evolve(100000,numParallel = 10, printEvery = 20, maxTarget = Some(maxPossibleScore), updateBest = onBest).as(())

  def hamming(lhs: Int, rhs: Int): Int = java.lang.Integer.bitCount(lhs ^ rhs)
  extension(i: Int)
    def bitCount: Int = java.lang.Integer.bitCount(i)