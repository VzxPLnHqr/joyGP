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