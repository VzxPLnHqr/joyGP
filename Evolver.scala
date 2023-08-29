package joygp

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scodec.bits._

import spire.math._

object Evolver {
    def iterateOnce[A](fitness: A => IO[BigInt])
                  (currentPop: List[BitVector], newPopSize: Int)
                  (implicit genetic: Genetic[A], randomIO: std.Random[IO]): IO[List[BitVector]] = for {
        
        // apply fitness function to each member of population
        // returns List[A,BigInt]
        scoredPop <- currentPop.parTraverse{ 
                        candidate_bytes => for {
                            repr <- IO(genetic.fromBits(candidate_bytes))
                            score <- fitness(repr)
                        } yield (candidate_bytes,score)
                    }

        medianScore <- IO(scoredPop).map(_.map(_._2).sorted).map(_.apply(currentPop.size / 2 - 1))
        _ <- IO.println(s"******Median score: $medianScore")

        // now build a new population by sampling from the old one
        // predetermined/fixed population size
        newPop <- (1 to newPopSize).toList.parTraverse{ 
                    i => (for {
                        parents <- sampleFromWeightedList(scoredPop)(randomIO)
                                        .both(sampleFromWeightedList(scoredPop)(randomIO))
                        crossed <- crossover(parents._1._1,parents._2._1)(randomIO)
                        mutated <- mutate(crossed)(randomIO)
                        score <- IO(genetic.fromBits(mutated)).flatMap(repr => fitness(repr))
                        //improvement <- IO.println(score - medianScore)
                        // select the fittest between the mutant and the parents
                        //selected <- IO(List((parents._1._1, parents._1._2), (parents._2._1, parents._2._2), (mutated,score))).map(_.maxBy(_._2))
                    } yield (mutated,score)).iterateUntil(_._2 >= medianScore).map(_._1)
                }
    } yield newPop

    def evolveN[A](fitness: A => IO[BigInt])
                  (startingPop: List[BitVector], numGenerations: Int, printEvery: Int = 10)
                  (implicit genetic: Genetic[A], randomIO: std.Random[IO]): IO[List[BitVector]] = for {
                    _ <- IO.unit
                    evolveOnce = (cur_pop:List[BitVector]) => iterateOnce(fitness)(cur_pop, newPopSize = 100)(genetic,randomIO)
                    finalPop <- (1 to numGenerations).toList.foldLeftM(startingPop){
                        case (newPop, i) => if( i % printEvery == 0) {
                            evolveOnce(newPop).flatTap(_ => printGenerationSummary(newPop,i).start)
                        } else {
                            evolveOnce(newPop)
                        }
                    }
                  } yield finalPop

    def printGenerationSummary(pop: List[BitVector], generationNumber: Int): IO[Unit] =
        IO.println(s"======= Generation $generationNumber ==========")

    /**
      * select a random big integer
      * (currently uses scala.util.Random)
      *
      * @param minInclusive
      * @param maxExclusive
      * @param randomIO
      * @return
      */
    def betweenBigInt(minInclusive: BigInt, maxExclusive: BigInt)
        (implicit randomIO: std.Random[IO]): IO[BigInt] = for {
            diff <- IO(maxExclusive - minInclusive)
            bitlength <- IO(diff.bitLength)
            r <- IO(BigInt(bitlength,scala.util.Random)).iterateUntil(_ < diff)
        } yield minInclusive + r

    /**
     *  Random selection from weighted list.
      * A solution that runs in O(n) would be to start out with selecting the first element. 
      * Then for each following element either keep the element you have or replace 
      * it with the next one. Let w be the sum of all weights for elements considered 
      * so far. Then keep the old one with probability w/(w+x) and choose the new 
      * one with p=x/(w+x), where x is the weight of the next element.
      * Source: https://stackoverflow.com/questions/4511331/randomly-selecting-an-element-from-a-weighted-list
      *
      * @param pop
      * @return
      */
    def sampleFromWeightedList[A]( pop: List[(A,BigInt)])
                                 (implicit randomIO: std.Random[IO]): IO[(A,BigInt)] =
        pop.foldLeftM((BigInt(0),Option.empty[(A,BigInt)])){ 
            case ((accum_score, incumbent), (candidate, candidate_score)) => 
                for {
                    new_accum_score <- IO(accum_score + candidate_score)
                    winner <- if(new_accum_score == 0)
                                IO(Some((candidate,candidate_score)))
                              else
                                betweenBigInt(0, new_accum_score)
                                .map(_ < candidate_score)
                                .ifM(
                                    ifTrue = IO(Some((candidate,candidate_score))),
                                    ifFalse = IO(incumbent)
                                )
                } yield (new_accum_score, winner)
        }.flatMap(r => (IO.fromOption(r._2)(new RuntimeException("No candidate selected!"))))

    /**
     * perform random naive crossover between two byte vectors:
        1. for each individual pick a crossover point
        3. take the head of lhs and tail of rhs
     * */
    def crossover(lhs: BitVector, rhs: BitVector)
        (implicit random: std.Random[IO]): IO[BitVector] = for {
            i <- random.betweenInt(0,lhs.size.toInt + 1)
            j <- random.betweenInt(0,rhs.size.toInt + 1)
            bits <- IO(lhs.take(i)).map(_ ++ rhs.drop(j))
            //_ <- IO.println(bytes.size - lhs.size)
        } yield bits

    /**
     * naive mutation
     * to the length of the gnome */
    def mutate(input: BitVector)(implicit random: std.Random[IO]): IO[BitVector] = {
        val size = input.size
        val probOfMutation = 0.01 // FIXME, just a fixed percentage for now
        val numBitsToMutate = (probOfMutation * input.size).ceil.toInt
        val indices = (0 until numBitsToMutate).toList.traverse(i => random.betweenLong(0,size+1))
        indices.map(_.foldLeft(input){
            (accum, i) => flipBit(accum,i)
        })
    }
    def flipBit(bits: BitVector, i: Long): BitVector = bits.get(i) match {
        case true => bits.clear(i)
        case false => bits.set(i)
    }

    def mutateByte(p: Double, b: Byte)(implicit random: std.Random[IO]): IO[Byte] = 
        random.nextDouble.map(_ <= p).flatMap{ 
                case true => random.nextBytes(1).map(_.head)
                case false => IO(b)
    }
}


