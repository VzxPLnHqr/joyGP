package joygp.examples

import joygp.{given, *}
import JoyBool.*
import cats.effect._
import cats.effect.std._
import cats.syntax.all._
import scala.concurrent.duration._
import scodec.bits.*
import spire.math.UInt
import spire.implicits.*
import java.io.*
import joygp.Genetic.Decoded
import joygp.Evolvable.*

class ReverseInput( inputBitLength: Int )(using Random[IO]):
  val readInput: Program = List.range(0,inputBitLength).foldLeft(id)((accum,_) => accum + readBit)
  val geneticProgram = new Genetic[Program] {
    def fromBits(genome: BitVector): Decoded[Program] = 
      Decoded(Program.parse(genome,stdLibrary),BitVector.empty)
  }

  /** program which will be evolved **/
  val geneticReadThenReverse: Genetic[Program] = Genetic.point(readInput).combine(geneticProgram)

  case class TestCase(initialState: ProgramState, expectedFinalState: ProgramState)

  def mkExample(input: BitVector): TestCase = TestCase(
    initialState = ProgramState().copy(input = input),
    expectedFinalState = ProgramState().copy(output = input.reverse)
  )

  def generateTestCases(n: Int): IO[List[TestCase]] =
    List.range(0,n).parTraverse(_ => Random[IO].nextBytes(inputBitLength/8).map(BitVector(_)).map(mkExample(_)))

  val numTestCases = 1

  def score(expected: ProgramState, candidate: ProgramState): BigInt = {
    val twoPow256 = BigInt(2).pow(256)
    //val opcount = candidate.opcount
    //val execSizeDiff = math.abs(expected.exec.size - candidate.exec.size)
    //val altSizeDiff = math.abs(expected.alt.size - candidate.alt.size)
    //val inputSizeDiff = math.abs(expected.input.size - candidate.input.size)
    val outputSizeDiff = math.abs(expected.output.size - candidate.output.size)
    val outputValueDiff = if( candidate.output.size  <= expected.output.size )
                            // pad the candidate to the output size
                            BigInt(signum = 1, expected.output.xor(candidate.output.padLeft(expected.output.size)).bytes.toArray)
                          else // if (candidate.output.size > expected.output.size )
                            //twoPow256 + BigInt(2).pow(outputSizeDiff.toInt)
                            BigInt(signum = 1, candidate.output.xor(expected.output.padLeft(candidate.output.size)).bytes.toArray)

    -outputValueDiff
  }

  def fitness( candidate: Program): IO[BigInt] = {
    generateTestCases(numTestCases).flatMap{
      tests => tests.parTraverse{
        test => candidate(test.initialState).flatMap{
          candidateFinalState =>
            // calculating a score is expensive, and the IO.cede's help manage the workload
            IO(score(test.expectedFinalState,candidateFinalState))
        }
      }
    }
  }.map(scores => scores.max)

  val example = mkExample(BitVector.fromValidBin("01"*32))

  val maxPossibleScore: BigInt = BigInt(numTestCases)*score(example.expectedFinalState,example.expectedFinalState)

  val run = geneticReadThenReverse.evolvable(fitness).flatMap(_.evolve(
    numThreads = 5,
    target = Some(maxPossibleScore),
    onBest = (si => IO(BigDecimal(-si.score)).map(_.log(2)) >>= IO.println)
  ))



object ReverseInput extends IOApp.Simple {
  val run = Random.scalaUtilRandom[IO].toResource.use{ 
    case given Random[IO] => new ReverseInput(256).run.timeout(150.seconds)
  }
}