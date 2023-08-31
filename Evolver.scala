package joygp

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scodec.bits._

import spire.math._

object Evolver {
    final case class ScoredIndividual(indiv: BitVector, score: BigInt) {
        override def toString = s"ScoredIndividual($score, ${indiv.toHex})"
    }
    
    /** apply fitness function to each member of population */
    def scorePop[A](pop: List[BitVector], fitness: A => IO[BigInt])
                    (implicit genetic: Genetic[A]): IO[List[ScoredIndividual]] = 
                        pop.parTraverse{ 
                            candidate_bytes => for {
                                repr <- IO(genetic.fromBits(candidate_bytes))
                                score <- fitness(repr)
                            } yield ScoredIndividual(candidate_bytes,score)
                        }

    def iterateOnce[A](fitness: A => IO[BigInt])
                  (scoredPop: List[ScoredIndividual], newPopSize: Int)
                  (implicit genetic: Genetic[A], randomIO: std.Random[IO]): IO[List[ScoredIndividual]] = for {

        medianScore <- IO(scoredPop).map(_.sortBy(_.score)).map(_.apply(scoredPop.size / 2).score)
        // now build a new population by sampling from the old one
        // predetermined/fixed population size
        // always keep the top candidate from current population
        newPop <- (1 to newPopSize).toList.parTraverse{ 
                    i => (for {
                        parents <- sampleFromWeightedList(scoredPop)(randomIO)
                                        .both(sampleFromWeightedList(scoredPop)(randomIO))
                        crossed <- crossover(parents._1.indiv,parents._2.indiv)(randomIO)
                        mutated <- mutate(crossed)(randomIO)
                        score <- IO(genetic.fromBits(mutated)).flatMap(repr => fitness(repr))
                        //improvement <- IO.println(score - medianScore)
                        // select the fittest between the mutant and the parents
                        // selected <- IO(List(parents._1, parents._2, ScoredIndividual(mutated,score))).map(_.maxBy(_.score))
                    } yield ScoredIndividual(mutated,score)).iterateUntil(_.score >= medianScore)
                }
    } yield newPop

    def evolveN[A](fitness: A => IO[BigInt])
                  (startingPop: List[ScoredIndividual], numGenerations: Int, numParallel: Int, printEvery: Int = 10, maxTarget: Option[BigInt] = None)
                  (implicit genetic: Genetic[A], randomIO: std.Random[IO]): IO[List[ScoredIndividual]] = for {
                    _ <- IO.unit
                    evolveOnce = (cur_pop:List[ScoredIndividual]) => iterateOnce(fitness)(cur_pop, newPopSize = 100)(genetic,randomIO).raceN(numParallel)
                    finalPop <- (1 to numGenerations).toList.foldLeftM(startingPop){
                        case (newPop, i) => if( i % printEvery == 0) {
                            evolveOnce(newPop).flatTap(_ => printGenerationSummary(newPop,i,maxTarget).start)
                        } else {
                            evolveOnce(newPop)
                        }
                    }
                  } yield finalPop

    def printGenerationSummary(scoredPop: List[ScoredIndividual], generationNumber: Int, maxTarget: Option[BigInt]): IO[Unit] = for {
        _ <- IO.println(s"======= Generation $generationNumber ==========")
        _ <- IO.println(s"*****Pop size: ${scoredPop.size}")
        _ <- IO.println(s"******Target score: $maxTarget")
        topCandidate <- IO(scoredPop.maxBy(_.score))
        longestCandidate <- IO(scoredPop.maxBy(_.indiv.length))
        medianScore <- IO(scoredPop.map(_.score)).map(_.apply(scoredPop.size / 2 - 1))
        diffFromMedian <- IO(topCandidate.score - medianScore)
        possibleImprovement <- IO(maxTarget).map(_.map(_ - topCandidate.score))
        _ <- IO.println(s"******Median score: $medianScore")
        _ <- IO.println(s"********Best Score: ${topCandidate.score} ($diffFromMedian from median)")
        _ <- IO.println(s"*********Possible Improvement: $possibleImprovement")
        _ <- IO.println(s"*******Longest (${longestCandidate.indiv.size} bits)")
        _ <- IO.println(s"********* Best (${topCandidate.indiv.size} bits): ${topCandidate.indiv.toHex}")
    } yield ()
        

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
    def sampleFromWeightedList[A]( scoredPop: List[ScoredIndividual])
                                 (implicit randomIO: std.Random[IO]): IO[ScoredIndividual] =
        scoredPop.foldLeftM((BigInt(0),Option.empty[ScoredIndividual])){ 
            case ((accum_score, incumbent), candidate) => 
                for {
                    new_accum_score <- IO(accum_score + candidate.score)
                    winner <- if(new_accum_score == 0)
                                IO(Some(candidate))
                              else
                                betweenBigInt(0, new_accum_score)
                                .map(_ < candidate.score)
                                .ifM(
                                    ifTrue = IO(Some(candidate)),
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
        val indices = (0 until numBitsToMutate).toList.traverse(i => random.betweenLong(0,size))
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

    /** race a list of IOs and return first
     * source: https://github.com/paul-snively/easyracer/commit/0642a62131e9127b28f32d781913b366539601e0#diff-9676066bc2a39fc2e916043cdb6c565c7ddce8fb8d85c17ea44dd7b2d193a6b6R94
     * // Fabio Labella's multiRace.
     * */
    def multiRace[F[_]: Concurrent, A](fas: List[F[A]]): F[A] = {
        def spawn[B](fa: F[B]): Resource[F, Unit] =
        Resource.make(fa.start)(_.cancel).void

        def finish(fa: F[A], d: Deferred[F, Either[Throwable, A]]): F[Unit] =
        fa.attempt.flatMap(d.complete).void

        Deferred[F, Either[Throwable, A]]
        .flatMap { result =>
            fas
            .traverse(fa => spawn(finish(fa, result)))
            .use(_ => result.get.rethrow)
        }
    }
    
    extension[F[_],A](effect: F[A])(using Concurrent[F])
        /** replicate `n` times and take first result **/
        def raceN(n: Int): F[A] = multiRace(List.fill(n)(effect))
}


