package joygp

import scodec.bits._
import cats.{Monad, Monoid}
import cats.syntax.all.*


trait Genetic[A]:
    /**
      * construct a value of type `A` from
      * genome represented as bitvector
      *
      * @param genome
      * @return
      */
    def fromBits(genome: BitVector): Genetic.Decoded[A]

object Genetic:
  final case class Decoded[A](value: A, remainingGenome: BitVector)

  def apply[A : Genetic]: Genetic[A] = summon[Genetic[A]]

  def point[A](a: A): Genetic[A] = new Genetic[A] {
    def fromBits(genome: BitVector): Decoded[A] = Decoded(a,genome)
  }

  def fmap[A,B](ga: Genetic[A])(f: A => Genetic[B]): Genetic[B] = new Genetic[B] {
    def fromBits(genome: BitVector): Decoded[B] =
      val Decoded(a,remainingGenome) = ga.fromBits(genome)
      f(a).fromBits(remainingGenome)
  }

  def map[A,B](ga: Genetic[A])(f: A => B): Genetic[B] = new Genetic[B] {
    def fromBits(genome: BitVector): Decoded[B] =
      val Decoded(a,remaining) = ga.fromBits(genome)
      Decoded(f(a),remaining)
  }

  def tupled[A,B](ga: Genetic[A], gb: Genetic[B]): Genetic[(A,B)] = new Genetic[(A,B)] {
    def fromBits(genome: BitVector): Decoded[(A, B)] =
      val decodedA = ga.fromBits(genome)
      val decodedB = gb.fromBits(decodedA.remainingGenome)
      Decoded((decodedA.value, decodedB.value),decodedB.remainingGenome)
  }

  /** concrete genomes **/
  def int32: Genetic[Int] = new Genetic[Int] {
    def fromBits(genome: BitVector): Decoded[Int] = Decoded(genome.take(32).toInt(), genome.drop(32))
  }

  def double64: Genetic[Double] = new Genetic[Double] {
    def fromBits(genome: BitVector): Decoded[Double] = 
      Decoded(java.nio.ByteBuffer.wrap(genome.take(64).padLeft(64).bytes.toArray).getDouble(), genome.drop(64))
  }

  // max size (in bits) followed by the string itself
  def stringUtf8: Genetic[(Int,String)] = new Genetic[(Int,String)] {
    def fromBits(genome: BitVector): Decoded[(Int,String)] =
      // first we decode an integer
      val Decoded(size,remaining) = int32.fromBits(genome)
      val decodedString = String(remaining.take(size).bytes.toArray)
      Decoded((size,decodedString), remaining.drop(size))
  }

  /** instances **/

  given geneticMonad : Monad[Genetic] with
    def pure[A](x: A): Genetic[A] = Genetic.point(x)
    def flatMap[A, B](fa: Genetic[A])(f: A => Genetic[B]): Genetic[B] = Genetic.fmap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => Genetic[Either[A, B]]): Genetic[B] = new Genetic[B] {
      def fromBits(genome: BitVector): Decoded[B] = f(a).fromBits(genome) match {
        case Decoded(Left(a1), remainingGenome) => tailRecM(a1)(f).fromBits(remainingGenome)
        case Decoded(Right(b),remainingGenome) => Decoded(b,remainingGenome)
      }
    }

  given geneticMonoid[A : Monoid]: Monoid[Genetic[A]] with
    def empty: Genetic[A] = Genetic.point(Monoid[A].empty)
    def combine(x: Genetic[A], y: Genetic[A]): Genetic[A] =
      x.flatMap(lhs => y.map(rhs => Monoid[A].combine(lhs,rhs)))