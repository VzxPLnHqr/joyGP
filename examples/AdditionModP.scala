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
    val execStackSizeDiff = math.abs(expected.exec.size - candidate.exec.size).toInt
    val inputStackSizeDiff = math.abs(expected.input.size - candidate.input.size).toInt
    val outputSizeDiff = math.abs(expected.output.size - candidate.output.size).toInt
    val outputValueDiff = if(outputSizeDiff == 0)
                            math.abs(expected.output.toInt() - candidate.output.toInt())
                          else
                            Int.MaxValue + 1 // if size is incorrect, maximum penalty, but less 1 to avoid neg infinity
    List(
      math.log(Int.MaxValue.toDouble - execStackSizeDiff),
      math.log(Int.MaxValue.toDouble - inputStackSizeDiff),
      math.log(Int.MaxValue.toDouble - outputSizeDiff),
      math.log(Int.MaxValue.toDouble - outputValueDiff)
    ).sum
  }

  val maxPossibleScore:Double = score(example.expectedFinalState,example.expectedFinalState)*numTestCases

  val randomIO: IO[std.Random[IO]] = std.Random.scalaUtilRandom

  val startingPop: IO[List[ScoredIndividual[Double]]] = List.range(0,100).parTraverse{ i => 
    for {
      size <- randomIO.flatMap(_.betweenInt(5,100))
      bits <- randomIO.flatMap(_.nextBytes(size)).map(BitVector(_))
      indiv <- IO(Program.parse(bits))
      score <- fitness(indiv)
    } yield ScoredIndividual(bits,score)
  }

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