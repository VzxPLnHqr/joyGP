package joygp

import cats.kernel.Order
import cats.effect._
import cats.effect.std._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scodec.bits._

import spire.math._
import spire.algebra.Ring
import spire.syntax.ring.{additiveGroupOps,additiveSemigroupOps}

final case class ScoredIndividual[B](indiv: BitVector, score: B) {
    override def toString = s"ScoredIndividual($score, ${indiv.toHex})"
}

trait Evolver:
    
    val throttle: Semaphore[IO]
    extension[A](fa: IO[A])
        def throttled:IO[A] = throttle.permit.use(_ => fa)

    extension[B](si: ScoredIndividual[B])
        def reScore[A](fitness: A => IO[B])(using genetic: Genetic[A]): IO[ScoredIndividual[B]] =
            IO(genetic.fromBits(si.indiv)).flatMap(repr => fitness(repr.value).throttled).map(score => ScoredIndividual(si.indiv,score))

    
    /** apply fitness function to each member of population */
    def scorePop[A,B](pop: List[BitVector], fitness: A => IO[B])
                    (implicit genetic: Genetic[A], ord: Order[B]): IO[List[ScoredIndividual[B]]] = 
                        pop.parTraverse{ 
                            candidate_bytes => for {
                                repr <- IO(genetic.fromBits(candidate_bytes))
                                score <- fitness(repr.value).throttled
                            } yield ScoredIndividual(candidate_bytes,score)
                        }

    def iterateOnce[A,B](fitness: A => IO[B])
                  (scoredPop: List[ScoredIndividual[B]], newPopSize: Int, updateBest: ScoredIndividual[B] => IO[Unit])
                  (using genetic: Genetic[A], supervisor: Supervisor[IO], randomIO: std.Random[IO], ord: Order[B], ring: Ring[B], randbetween: RandBetween[B]): IO[List[ScoredIndividual[B]]] = for {
        
        // find current best candidate and notify caller via updateBest callback
        best <- IO(scoredPop).map(_.maxBy(_.score)).flatMap(_.reScore(fitness)).flatTap(si => supervisor.supervise(updateBest(si)))
        
        // now build a new population by sampling from the old one
        // predetermined/fixed population size
        newPop <- (1 to newPopSize).toList.parTraverse{ 
                    i => (for {
                        // select parents
                        parents <- sampleFromTournament(scoredPop)(ord,randomIO).flatMap(_.reScore(fitness))
                                        .both(
                                            sampleFromTournament(scoredPop)(ord,randomIO).flatMap(_.reScore(fitness))
                                                // mutate this parent
                                                // .flatMap(si => mutate(si.indiv)(randomIO).map(bits => (bits,genetic.fromBits(bits))))
                                                //    .flatMap((bits,repr) => fitness(repr).map(s => ScoredIndividual(bits,s)))
                                        )
                            /*sampleFromWeightedList(scoredPop)(ring,ord,randbetween,randomIO)
                                        .both(
                                            sampleFromWeightedList(scoredPop)(ring,ord,randbetween,randomIO)
                                                //.flatMap(si => mutate(si.indiv)(randomIO).map(bits => (bits,genetic.fromBits(bits))))
                                                //    .flatMap((bits,repr) => fitness(repr).map(s => ScoredIndividual(bits,s)))
                                            )*/
                        crossed <- crossover(parents._1.indiv,parents._2.indiv)
                        mutated <- mutate(crossed)
                        score <- (IO(genetic.fromBits(mutated)) <* IO.cede).flatMap(repr => fitness(repr.value).throttled).guarantee(IO.cede)
                        // select the fittest between the mutant and the parents
                        //selected <- IO(List(parents._1, parents._2, ScoredIndividual(mutated,score))).map(_.maxBy(_.score))
                        selected <- IO(ScoredIndividual(mutated,score))
                    } yield selected) //.iterateUntil(_.score >= medianScore)
                }
    // always keep the top candidate from current population
    } yield best :: newPop
    //} yield newPop
    
    def evolveN[A,B](fitness: A => IO[B])
                  (startingPop: List[ScoredIndividual[B]], numGenerations: Int, 
                   numParallel: Int, printEvery: Int = 10, maxTarget: Option[B] = None,
                   updateBest: ScoredIndividual[B] => IO[Unit])
                  (using genetic: Genetic[A], supervisor: Supervisor[IO], randomIO: std.Random[IO], ord: Order[B], ring: Ring[B], randbetween: RandBetween[B]): IO[List[ScoredIndividual[B]]] = 
        def evolveOnce(cur_pop:List[ScoredIndividual[B]]) = iterateOnce(fitness)(cur_pop, newPopSize = startingPop.size, updateBest).raceN(numParallel)
        for {     
            finalpop <- (1 to numGenerations).toList.foldLeftM(startingPop){
                case (newPop, i) => if( i % printEvery == 0) {
                    evolveOnce(newPop) <* supervisor.supervise(printGenerationSummary(newPop,i,fitness,maxTarget))
                } else {
                    evolveOnce(newPop)
                }
            }
        } yield finalpop

    def printGenerationSummary[A : Genetic, B : Ring : Order](scoredPop: List[ScoredIndividual[B]], generationNumber: Int, fitness: A => IO[B], maxTarget: Option[B]): IO[Unit] = for {
        _ <- IO.println(s"======= Generation $generationNumber ==========")
        _ <- IO.println(s"*****Pop size: ${scoredPop.size}")
        reScoredPop <- scoredPop.parTraverse(_.reScore(fitness))
        topCandidate <- IO(reScoredPop.maxBy(_.score))
        longestCandidate <- IO(reScoredPop.maxBy(_.indiv.length))
        shortestCandidate <- IO(reScoredPop.minBy(_.indiv.length))
        medianScore <- IO(reScoredPop.map(_.score)).map(_.apply(scoredPop.size / 2 - 1))
        diffFromMedian <- IO(medianScore - topCandidate.score)
        possibleImprovement <- IO(maxTarget).map(_.map(_ - topCandidate.score))
        bestHex <- IO(if(topCandidate.indiv.size > 1000) "..." else topCandidate.indiv.toHex)
        _ <- IO.println(s"******Target score: $maxTarget")
        _ <- IO.println(s"********Best Score:      ${topCandidate.score} Possible Improvement: $possibleImprovement")
        _ <- IO.println(s"******Median score:      $medianScore ($diffFromMedian from best)")
        _ <- IO.println(s"*******Range [${shortestCandidate.indiv.size}, ${longestCandidate.indiv.size}] bits")
        _ <- IO.println(s"********* Best (${topCandidate.indiv.size} bits): $bestHex")
    } yield ()
        
object Evolver:
    def throttled(n: Long): IO[Evolver] = Semaphore[IO](n).map {
        theThrottle => new Evolver {
            val throttle = theThrottle
        }
    }

trait RandBetween[B]{
    def randBetween(minInclusive: B, maxExclusive: B)(using randomIO: std.Random[IO]): IO[B]
}
given RandBetween[Double] with
    def randBetween(minInclusive: Double, maxExclusive: Double)(using randomIO: std.Random[IO]): IO[Double] = randomIO.betweenDouble(minInclusive,maxExclusive)
given RandBetween[BigInt] with
    def randBetween(minInclusive: BigInt, maxExclusive: BigInt)(using randomIO: std.Random[IO]): IO[BigInt] = randomIO.betweenBigInt(minInclusive,maxExclusive)

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
def sampleFromWeightedList[A,B : Ring : Order : RandBetween ]( scoredPop: List[ScoredIndividual[B]])
                                (implicit randomIO: std.Random[IO]): IO[ScoredIndividual[B]] =
    scoredPop.foldLeftM((Ring[B].zero,Option.empty[ScoredIndividual[B]])){ 
        case ((accum_score, incumbent), candidate) => 
            for {
                new_accum_score <- IO(accum_score + candidate.score)
                winner <- if(new_accum_score == 0)
                            IO(Some(candidate))
                            else
                            //betweenBigInt(0, new_accum_score)
                            implicitly[RandBetween[B]].randBetween(Ring[B].zero, new_accum_score)
                            .map(_ < candidate.score)
                            .ifM(
                                ifTrue = IO(Some(candidate)),
                                ifFalse = IO(incumbent)
                            )
            } yield (new_accum_score, winner)
    }.flatMap(r => (IO.fromOption(r._2)(new RuntimeException("No candidate selected!"))))

/**
 * Tournament Selection. Tournament selection returns the fittest individual 
 * of some t individuals picked at random, with replacement, from the population. 
 * First choose t (the tournament size) individuals from the population at random. 
 * Then choose the best individual from tournament with probability p, choose 
 * the second best individual with probability p*(1-p), choose the third best 
 * individual with probability p*((1-p)2), and so on... Tournament Selection has 
 * become the primary selection technique used for the Genetic Algorithm. First, 
 * it's not sensitive to the particulars of the fitness function. Second, 
 * it's very simple, requires no preprocessing, and works well with parallel 
 * algorithms. Third, it's tunable: by setting the tournament size t, you can 
 * change how selective the technique is. At the extremes, if t = 1, this is 
 * just random search. If t is very large (much larger than the population size 
 * itself), then the probability that the fittest individual in the population 
 * will appear in the tournament approaches 1.0, and so Tournament Selection 
 * just picks the fittest individual each time.
 * source: https://haifengl.github.io/api/java/smile/gap/Selection.html
 * */
def sampleFromTournament[B : Order](scoredPop: List[ScoredIndividual[B]])
                            (implicit randomIO: std.Random[IO]): IO[ScoredIndividual[B]] = {
    val size = 3 // hard-coded to tourney size 3
    val pFirst = 0.95
    val pSecond = pFirst*(1-pFirst)
    val pThird = pFirst*(math.pow(1-pFirst,2))
    (
        randomIO.elementOf(scoredPop), 
        randomIO.elementOf(scoredPop), 
        randomIO.elementOf(scoredPop),
        randomIO.nextDouble
    ).parFlatMapN(
        (fst,snd,thd,r) => IO(List(fst,snd,thd))
            .map(_.sortBy(_.score))
                .map { 
                    case c0 :: c1 :: c2 :: Nil => 
                        if(r <= pThird) c0
                        else if(r <= pSecond) c1
                        else c2
                    case _ => throw new RuntimeException("impossible pattern")
                }
    )
}
/**
 * perform random naive crossover between two byte vectors:
    1. for each individual pick a crossover point
    3. take the head of lhs and tail of rhs
    * */
def crossover(lhs: BitVector, rhs: BitVector)
    (using random: std.Random[IO]): IO[BitVector] = for {
        i <- random.betweenInt(0,lhs.size.toInt + 1)
        j <- random.betweenInt(0,rhs.size.toInt + 1)
        bits <- IO.blocking(lhs.take(i)).map(_ ++ rhs.drop(j))
    } yield bits

/**
 * naive mutation
 * to the length of the gnome */
def mutate(input: BitVector)(using random: std.Random[IO]): IO[BitVector] = {
    val size = input.size
    val probOfMutation = 0.01 // FIXME, just a fixed percentage for now
    val numBitsToMutate = (probOfMutation * input.size).ceil.toInt
    val indices = IO.blocking((0 until numBitsToMutate).toList).flatMap(_.traverse(i => random.betweenLong(0,size)))
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


