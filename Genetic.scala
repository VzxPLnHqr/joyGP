package joygp

import scodec.bits._


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
