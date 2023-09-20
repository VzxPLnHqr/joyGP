package joygp

import cats._
import cats.instances.order.catsKernelOrderingForOrder
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import scala.concurrent.duration.*
import scodec.bits.*

class Evolvable[A,B](
  refCurrentPopulation: Ref[IO,List[ScoredIndividual[B]]],
  fitnessFunction: A => IO[B],
  genetic: Genetic[A])
  (using Random[IO], Order[B]):
  // type B represents the "score" used by the fitness function so
  // we need an Order[B] instance to be able to compare individuals

  def currentPopulation = refCurrentPopulation.get

  def currentBest = refCurrentPopulation.get.map(_.maxBy(_.score))

  def evolve( 
      numThreads: Int = 5, 
      target: Option[B] = None, 
      onBest: ScoredIndividual[B] => IO[Unit] = (_) => IO.unit)
      (using spire.algebra.Ring[B], RandBetween[B]
    ): IO[Unit] = (
      Backpressure[IO](Backpressure.Strategy.Lossy,1).toResource, 
      Random.scalaUtilRandom[IO].toResource,
      Supervisor[IO]
  ).tupled.use {
    case (
      given Backpressure[IO], 
      given Random[IO],
      given Supervisor[IO]) =>
        currentPopulation.flatMap {
          startingPop => 
            Evolver.throttled(numThreads).flatMap { evolver => 
              evolver.evolveN(fitnessFunction)(
                      startingPop = startingPop, 
                      numGenerations = 100000, 
                      numParallel = 1, 
                      printEvery = 10, 
                      maxTarget = target, 
                      updateBest = si => onBest(si))(using genetic)
              }
        }.background.useForever
  }

object Evolvable:
  extension[A](ga: Genetic[A])
    def evolvable[B: Order](fitnessFunction: A => IO[B]): IO[Evolvable[A,B]] = for {
      random <- Random.scalaUtilRandom[IO]
      randomBitVectors <- random.bitVectors(howMany = 100, minSizeBytes = 5, maxSizeBytes = 100)
      refStartingPop <- Ref.ofEffect(randomBitVectors.traverse {
        bits => fitnessFunction(ga.fromBits(bits).value).map {
          score => ScoredIndividual(bits,score)
        }
      })
      res = new Evolvable[A,B](refStartingPop, fitnessFunction,ga)(using random)
    } yield res