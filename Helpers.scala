package joygp

import cats.effect.*
import cats.effect.std.Random
import scodec.bits.*
import cats.syntax.all.*

extension (random: Random[IO])
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

    def bitVectors(howMany: Int, minSizeBytes: Int = 5, maxSizeBytes: Int = 100): IO[List[BitVector]] = 
      List.range(0,howMany).parTraverse{ i => 
        for {
          size <-  random.betweenInt(minSizeBytes,maxSizeBytes)
          bits <-  random.nextBytes(size).map(BitVector(_))
        } yield bits
      }