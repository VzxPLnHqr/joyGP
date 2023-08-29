package joygp

import scodec.bits._


trait Genetic[A]{
    /**
      * construct a value of type `A` from
      * genome represented as bitvector
      *
      * @param genome
      * @return
      */
    def fromBits(genome: BitVector): A
}
