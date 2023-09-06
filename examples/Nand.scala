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

object Nand extends IOApp.Simple:

  case class TestCase(initialState: ProgramState, expectedFinalState: ProgramState)
  
  def bool2bit(b: Boolean): BitVector = b match {
    case true => BitVector.one
    case false => BitVector.zero
  }

  def mkExample(lhs: Boolean, rhs: Boolean): TestCase = TestCase(
    initialState = ProgramState().copy(input = bool2bit(lhs) ++ bool2bit(rhs)),
    expectedFinalState = ProgramState().copy(output = bool2bit(lhs.nand(rhs)))
  )
  val example = mkExample(true,true)

  def generateTestCases(n: Int): IO[List[TestCase]] = List.range(0,n).traverse{
    i => for {
      rand <- randomIO
      lhs <- rand.nextBoolean
      rhs <- rand.nextBoolean
    } yield mkExample(lhs,rhs)
  }

  /** number of test cases to create for each individual assessment */
  val numTestCases: Int = 100

  val fitness: Program => IO[BigInt] = candidate => {
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
  def score(expected: ProgramState, candidate: ProgramState): BigInt = {
    val execStackSizeDiff = math.abs(expected.exec.size - candidate.exec.size)
    val inputStackSizeDiff = math.abs(expected.input.size - candidate.input.size)
    // if there are input bits which have not yet been read, we heavily penalize any output bits
    val outputSizeDiff =  if(inputStackSizeDiff > 0)
                            Int.MaxValue
                          else 
                            math.abs(expected.output.size - candidate.output.size)
    val outputValueDiff = if(outputSizeDiff == 0)
                              //UInt(expected.output.xor(candidate.output).toInt())
                              expected.output.xor(candidate.output).head match {
                                case true => UInt(1)
                                case false => UInt(0)
                              }
                          else
                              UInt.MaxValue
    List(
      BigInt(Int.MaxValue - execStackSizeDiff),
      BigInt(Int.MaxValue - inputStackSizeDiff),
      BigInt(Int.MaxValue - outputSizeDiff).ensuring(_ >= 0),
      (UInt.MaxValue - outputValueDiff).toBigInt.ensuring(_ >= 0) // if output size not same, this component is zero
    ).sum
  }.ensuring(_ > 0)

  val maxPossibleScore:BigInt = score(example.expectedFinalState,example.expectedFinalState)*numTestCases

  val randomIO: IO[std.Random[IO]] = std.Random.scalaUtilRandom

  //val bestYet = IO("").map(hex => BitVector.fromValidHex(hex)).flatMap(bits => fitness(Program.parse(bits)).map(score => ScoredIndividual(bits,score)))
  
  //val startingPop = bestYet.map(best => List.fill(100)(best))
  val startingPop: IO[List[ScoredIndividual[BigInt]]] = List.range(0,100).parTraverse{ i => 
    for {
      size <- randomIO.flatMap(_.betweenInt(5,100))
      bits <- randomIO.flatMap(_.nextBytes(size)).map(BitVector(_))
      indiv <- IO(Program.parse(bits))
      score <- fitness(indiv)
    } yield ScoredIndividual(bits,score)
  } // .flatMap(xs => bestYet.map(_ :: xs.tail))
  
  val refTheBest = Ref.of[IO,ScoredIndividual[BigInt]](ScoredIndividual(BitVector.empty,0))
  //val tolerance = math.pow(10,-14)
  def onBest(si: ScoredIndividual[BigInt]): IO[Unit] = for {
    ref <- refTheBest
    current <- ref.get
    _ <- if((si.score - maxPossibleScore).abs == 0) 
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
              maxTarget: Option[BigInt] = None,
              updateBest: ScoredIndividual[BigInt] => IO[Unit] = si => IO.unit)
              (implicit ord: cats.kernel.Order[BigInt], ring: spire.algebra.Ring[BigInt], randbetween: RandBetween[BigInt])
              :IO[List[ScoredIndividual[BigInt]]] = for { 
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

  def fx(candidate: Program): Boolean => Boolean => IO[Boolean] = lhs => rhs => {
    val psi = ProgramState().copy(input = bool2bit(lhs) ++ bool2bit(rhs))
    candidate(psi).map(_.output.head)
  }

  //override protected def blockedThreadDetectionEnabled = true
  def run: IO[Unit] = evolve(100000,numParallel = 1, printEvery = 20, maxTarget = Some(maxPossibleScore), updateBest = onBest).as(())

  def hamming(lhs: Int, rhs: Int): Int = java.lang.Integer.bitCount(lhs ^ rhs)
  extension(i: Int)
    def bitCount: Int = java.lang.Integer.bitCount(i)