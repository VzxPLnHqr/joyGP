package joygp

import cats.effect.IO
import scodec.bits.{ByteVector,BitVector}

object JoyBool:

  type Stack[T] = List[T]

  sealed trait Program {
    /** the "size" of the program **/
    def size: Long = 1 // all programs have size 1 unless overridden
    def effect: ProgramState => IO[ProgramState]
    def apply(ps: ProgramState): IO[ProgramState] = effect(ps)
  }
  case class Quoted(p: Program) extends Program {
    override def size = p.size + 1 // add 1 to the quoted program
    def effect: ProgramState => IO[ProgramState] =
      // the effect of a quotation is to push itself onto the exec stack
      state => IO(state.copy(exec = Quoted(p) :: state.exec, opcount = state.opcount + 1))
  }
  def Effect(f: ProgramState => IO[ProgramState]): Program = new Program {
    def effect: ProgramState => IO[ProgramState] = f
  }

  def Effect(name: String)( f: ProgramState => IO[ProgramState]): Program = new Program {
    def effect = state => f(state).map(psf => psf.copy(opcount = psf.opcount + 1))
  }

  object Program:
    // programs are a monoid!
    def combine(lhs: Program, rhs: Program): Program = (lhs,rhs) match {
      case (Quoted(b), Quoted(a)) => new Program {
        override def size = Quoted(b).size + Quoted(a).size
        // quotations just get pushed onto stack
        def effect = state => IO(state.copy(exec = Quoted(a) :: Quoted(b) :: state.exec)).map(ps => ps.copy(opcount = ps.opcount + 1))
      }
      case (Quoted(a), p: Program) => new Program{
        override def size = Quoted(a).size + p.size
        // try to execute the effect
        def effect = state => p.effect(state.copy(exec = Quoted(a) :: state.exec)).map(ps => ps.copy(opcount = ps.opcount + 1))
      }
      case (f:Program, Quoted(a)) => new Program {
        override def size: Long = f.size + Quoted(a).size
        // execute f and then push Quoted(a) onto the stack
        def effect = 
          state => 
            f(state).map(afterF => afterF.copy(exec = Quoted(a) :: afterF.exec)).map(ps => ps.copy(opcount = ps.opcount + 1))
      }
      case (g:Program, f:Program) => new Program {
        // execute f after g
        // seem to need to lift g into IO to avoid stackoverflow
        // this might be known as trapolining, not sure, but it seems to work
        def effect = state => IO(g).flatMap(_.effect(state)).flatMap(ps => f.effect(ps)).map(ps => ps.copy(opcount = ps.opcount + 1))
        override def size: Long = g.size + f.size
      }
    }
    def parse(bits: BitVector, library: Quoted = stdLibrary): Program = {
      @annotation.tailrec
      def inner(prog: Program, remaining: BitVector): Program = remaining.headOption match {
        case Some(bit) => inner(prog + fromBit(bit,library), remaining.drop(1))
        case None => prog // we are done
      }
      inner(id,bits)
    }
    def parse(bytes: ByteVector): Program = parse(bytes.bits) 
    def fromBit(bit: Boolean, library: Quoted): Program = bit match {
      // 1 == k
      case true => conservativeK // k
      // 0 == [library] [z] onto the stack
      case false => library + Quoted(conservativeZ)
    }
    def fromValidHex(numBits: Long, hex: String): Program = parse(BitVector.fromValidHex(hex).dropRight(hex.size*4 - numBits))
    def fromValidBin(bin: String): Program = parse(BitVector.fromValidBin(bin))
    def apply(bin: String): Program = fromValidBin(bin)
    def rand(numBytes: Int): Program = parse(ByteVector(scala.util.Random.nextBytes(numBytes)).bits)

    given geneticJoyBool: Genetic[Program] with {
      def fromBits(genome: BitVector): Program = Program.parse(genome,stdLibrary)
    }

  extension (lhs: Program)
    def +(rhs: Program): Program = Program.combine(lhs,rhs)
  
  final case class ProgramState(
    // exec always contains the remaining code to be executed
    exec: Stack[Program] = Nil,
    alt: Stack[Program] = Nil,
    input: BitVector = BitVector.empty,
    output: BitVector = BitVector.empty,
    opcount: Long = 0
  ) {
    override def toString(): String =
      val stackSize = exec.length
      val altStackSize = alt.length
      s"\nProgramState(\n \topcount=$opcount \n \texec ($stackSize items)= ... \n \talt ($altStackSize items)= ... \n\tinput=$input \n \toutput=$output\n)"
  }

  extension(ps: ProgramState)
    def step: IO[ProgramState] = ps.stepMany(1)
    def stepMany(steps: Long): IO[ProgramState] = ps.exec match {
      case Nil => IO(ps) // we must be done, no more state changes
      case op :: tail if(steps > 1) => op.effect(ps.copy(exec = tail)).flatMap(_.stepMany(steps - 1))
      case _ => IO(ps)
    }
    def apply(prog: Program): IO[ProgramState] = prog(ps)

  // define elementary programs
  // most of the names taken from: http://tunes.org/~iepos/joy.html#appendix
  /** the "empty" program (a no-op) **/
  val id = new Program {
    override def size = 0 // empty program has zero size
    def effect: ProgramState => IO[ProgramState] = state => IO(state)
  }

  // nil == []
  val nil = Quoted(id)

  val i = Effect("i") { state => 
    // [A] i == A
    state.exec match {
      case Quoted(a) :: tail => a.effect(state.copy(exec = tail))
      case _ => IO(state) // noop
    }
  }
  val k = Effect("k") { state =>
    // [B] [A] k    == A
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => a.effect(state.copy(exec = tail))
      case _ => IO(state) // noop for anything else
    }
  }

  val z = Effect("z") { state =>
    // [B] [A] z    == B
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => b.effect(state.copy(exec = tail))
      case _ => IO(state) // noop for anything else
    }
  }

  val swap = Effect("swap") { state =>
    // [B] [A] swap == [A] [B]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => IO(state.copy(exec = Quoted(b) :: Quoted(a) :: tail))
      case _ => IO(state) // noop
    }
  }

  val dup = Effect("dup") { state => 
    // [A] dup == [A] [A]
    state.exec match {
      case Quoted(a) :: tail => IO(state.copy(exec = Quoted(a) :: Quoted(a) :: tail))
      case _ => IO(state) // noop
    }  
  }

  val zap = Effect("zap") { state =>
    // [A] zap ==
    state.exec match {
      case Quoted(a) :: tail => IO(state.copy(exec = tail))
      case _ => IO(state) // noop
    }

  }

  val cat = Effect("cat") { state => 
    // [B] [A] cat == [B A]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => IO(state.copy(exec = Quoted(b + a) :: tail))
      case _ => IO(state) // noop
    }  
  }

  val cons = Effect("cons") { state => 
    // [B] [A] cons == [[B] A]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => IO(state.copy(exec = Quoted(Quoted(b) + a) :: tail))
      case _ => IO(state) // noop
    }
  }

  val unit = Effect("unit") { state => 
    // [A] unit == [[A]]
    state.exec match {
      case Quoted(a) :: tail => IO(state.copy(exec = Quoted(Quoted(a)) :: tail))
      case _ => IO(state) // noop
    }
  }

  val dip = Effect("dip") { state => 
    // [B] [A] dip == A [B]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => 
        a.effect(state.copy(exec = tail)).map{ afterA =>
          afterA.copy(exec = Quoted(b) :: afterA.exec)
        }
      case _ => IO(state) // noop
    }
  }

  val cake = Effect("cake") { state =>
    // [B] [A] cake == [[B] A] [A [B]]
    state.exec match {
      case (Quoted(a) :: Quoted(b) :: tail) =>
        IO(state.copy(exec = Quoted(a + Quoted(b)) :: Quoted(Quoted(b) + a) :: tail))
      case _ => IO(state) // noop
    }
  }

  // define IO effects 
  val readBit = Effect("readBit") {
    state =>
      // pop the next bit from the input
      // if it is 1, push Quoted(k) onto the stack
      // otherwise push Quoted(z) onto the stack
        IO(state).map { ps => ps.input.headOption match {
          case Some(true) => ps.copy(exec = Quoted(conservativeK) :: ps.exec, input = ps.input.tail, output = ps.output)
          case Some(false) => ps.copy(exec = Quoted(conservativeZ) :: ps.exec, input = ps.input.tail, output = ps.output)
          case None => ps // input is empty, therefore noop
        }
      }
  }

  val putTrue = Effect("putTrue") {
    state =>
      IO.blocking(state.copy(output = (BitVector.one ++ state.output).compact))
  }
  val maybePutTrue = Effect("maybePutTrue") {
    state => state.input.isEmpty match {
      case true => putTrue.effect(state)
      case false => IO(state) // no-op if we have not finished reading input
    }
  }

  val putFalse = Effect("putFalse") {
    state =>
      IO.blocking(state.copy(output = (BitVector.zero ++ state.output).compact))
  }
  val maybePutFalse = Effect("maybePutFalse") {
    state => state.input.isEmpty match {
      case true => putFalse.effect(state)
      case false => IO(state) // no-op if e have not finished reading input
    }
  }

  val flush = Effect("flush") {
    state => IO(state.copy(exec = Nil))
  }

  val toAlt = Effect("toAlt") {
    state => state.exec match {
      case quotedA :: tail => IO(state.copy(exec = tail, alt = quotedA :: state.alt))
      case _ => IO(state) // no-op
    }
  }
  val fromAlt = Effect("fromAlt") {
    state => state.alt match {
      case quotedA :: tail => IO(state.copy(exec = quotedA :: state.exec, alt = tail))
      case _ => IO(state) // no-op
    }
  }

  val conservativeK = swap + toAlt + i
  val conservativeZ = swap + conservativeK

  // construct a "library" as a binary tree of quoted programs
  // note: this is a naive implementation which requires an even number
  // of programs
  def mkLibrary(leaves: List[Program]): Quoted = {
    require(leaves.nonEmpty, "library must not be empty")
    //require(leaves.size % 2 == 0, "number of programs in library must be even")

    @annotation.tailrec
    def buildTree(nodes: List[Quoted]): List[Quoted] = nodes match {
      case ns if (ns.size <= 1) => ns // we are at the root. Done
      case ns =>
        val pairedNodes = ns
          .grouped(2)
          .map {
            case List(lhs,rhs) => Quoted(lhs + rhs)
            case List(lhs) => lhs
            case _ => throw new IllegalArgumentException("should never be here")
          }.toList
        buildTree(pairedNodes)
    }
    buildTree(
      leaves.grouped(2).map {
        case List(lhs, rhs) => Quoted(Quoted(lhs) + Quoted(rhs))
        case List(lhs) => Quoted(lhs)
        case _ => throw new IllegalArgumentException("should never be here")
      }.toList
    ).head
  }

  val stdLibrary = mkLibrary(List(
    swap,
    //dup,
    //zap,
    cat,
    cons,
    unit,
    i,
    // k,
    conservativeK,
    conservativeZ,
    dip,
    nil,
    readBit,
    //putTrue,
    //putFalse,
    maybePutTrue,
    maybePutFalse,
    toAlt,
    fromAlt,
    // flush // equivalent to halting
  ))
    
  def doTheTest: IO[ProgramState] = {
    val initPS = ProgramState(input = BitVector.fill(64)(true))
    List.range(0,64).foldLeft(IO(initPS))((accumulatedStateIO, _) => accumulatedStateIO.flatMap(readBit(_)))
  }