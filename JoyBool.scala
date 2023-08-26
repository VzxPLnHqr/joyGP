package joygp

import scodec.bits.{ByteVector,BitVector}

object JoyBool:

  type Stack[T] = List[T]

  sealed trait Program {
    def expression: String
    def effect: ProgramState => ProgramState
    override def toString = expression
    def apply(ps: ProgramState): ProgramState = effect(ps)
  }
  case class Quoted(p: Program) extends Program {

    def expression: String = s"[$p]"
    def effect: ProgramState => ProgramState =
      // the effect of a quotation is to push itself onto the exec stack
      state => state.copy(exec = Quoted(p) :: state.exec)
  }
  case class Effect(expression: String, f: ProgramState => ProgramState) extends Program {
    def effect = f
  }
  def Effect(name: String)( f: ProgramState => ProgramState): Effect = Effect(name,f)

  // define how programs combine (it's a monoid!)
  extension (lhs: Program)
    def +(rhs: Program): Program = (lhs,rhs) match {
      case (Quoted(b), Quoted(a)) => Effect(s"[$b] [$a]") {
        // quotations just get pushed onto stack
        state => state.copy(exec = Quoted(a) :: Quoted(b) :: state.exec)
      }
      case (Quoted(a), Effect(exp,f)) => Effect(s"[$a] $exp") {
        // try to execute the effect
        state => f(state.copy(exec = Quoted(a) :: state.exec))
      }
      case (Effect(expg,g), Effect(expf,f)) => Effect(s"$expg $expf") {
        // execute f after g
        state => f(g(state))
      }
      case (Effect(exp,f), Quoted(a)) => Effect(s"$exp [$a]") {
        // execute f and then push Quoted(a) onto the stack
        state => 
          val afterF = f(state)
          afterF.copy(exec = Quoted(a) :: afterF.exec)
      }
    }

  // define elementary programs
  // most of the names taken from: http://tunes.org/~iepos/joy.html#appendix
  val id = Effect("")(state => state)

  // nil == []
  val nil = Quoted(id)

  val i = Effect("i") { state => 
    // [A] i == A
    state.exec match {
      case Quoted(a) :: tail => a.effect(state.copy(exec = tail))
      case _ => state // noop
    }
  }
  val k = Effect("k") { state =>
    // [B] [A] k    == A
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => a.effect(state.copy(exec = tail))
      case _ => state // noop for anything else
    }
  }

  val z = Effect("z") { state =>
    // [B] [A] z    == B
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => b.effect(state.copy(exec = tail))
      case _ => state // noop for anything else
    }
  }

  val swap = Effect("swap") { state =>
    // [B] [A] swap == [A] [B]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => state.copy(exec = Quoted(b) :: Quoted(a) :: tail)
      case _ => state // noop
    }
  }

  val dup = Effect("dup") { state => 
    // [A] dup == [A] [A]
    state.exec match {
      case Quoted(a) :: tail => state.copy(exec = Quoted(a) :: Quoted(a) :: tail)
      case _ => state // noop
    }  
  }

  val zap = Effect("zap") { state =>
    // [A] zap ==
    state.exec match {
      case Quoted(a) :: tail => state.copy(exec = tail)
      case _ => state // noop
    }

  }

  val cat = Effect("cat") { state => 
    // [B] [A] cat == [B A]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => state.copy(exec = Quoted(b + a) :: tail)
      case _ => state // noop
    }  
  }

  val cons = Effect("cons") { state => 
    // [B] [A] cons == [[B] A]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => state.copy(exec = Quoted(Quoted(b) + a) :: tail)
      case _ => state // noop
    }
  }

  val unit = Effect("unit") { state => 
    // [A] unit == [[A]]
    state.exec match {
      case Quoted(a) :: tail => state.copy(exec = Quoted(Quoted(a)) :: tail)
      case _ => state // noop
    }
  }

  val dip = Effect("dip") { state => 
    // [B] [A] dip == A [B]
    state.exec match {
      case Quoted(a) :: Quoted(b) :: tail => 
        val afterA = a.effect(state.copy(exec = tail))
        afterA.copy(exec = Quoted(b) :: afterA.exec)
      case _ => state // noop
    }
  }

  val cake = Effect("cake") { state =>
    // [B] [A] cake == [[B] A] [A [B]]
    state.exec match {
      case (Quoted(a) :: Quoted(b) :: tail) =>
        state.copy(exec = Quoted(a + Quoted(b)) :: Quoted(Quoted(b) + a) :: tail)
      case _ => state // noop
    }
  }

  // define IO effects 
  val readBit = Effect("readBit") {
    state =>
      // pop the next bit from the input
      // if it is 1, push Quoted(k) onto the stack
      // otherwise push Quoted(z) onto the stack
      state.input.headOption match {
        case Some(true) => state.copy(exec = Quoted(k) :: state.exec, input = state.input.drop(1))
        case Some(false) => state.copy(exec = Quoted(z) :: state.exec, input = state.input.drop(1))
        case None => state // input is empty, therefore noop
      }
  }

  val putTrue = Effect("putTrue") {
    state => 
      state.copy(output = state.output.+:(true))
  }

  val putFalse = Effect("putFalse") {
    state =>
      state.copy(output = state.output.+:(false))
  }

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
    dup,
    zap,
    cat,
    cons,
    unit,
    i,
    dip,
    nil,
    readBit,
    putTrue,
    putFalse
  ))

  final case class ProgramState(
    // exec always contains the remaining code to be executed
    exec: Stack[Program] = Nil,
    input: BitVector = BitVector.empty,
    output: BitVector = BitVector.empty
  )

  extension(ps: ProgramState)
    def step: ProgramState = ps.stepMany(1)
    def stepMany(steps: Long): ProgramState = ps.exec match {
      case Nil => ps // we must be done, no more state changes
      case op :: tail if(steps > 1) => op.effect(ps.copy(exec = tail)).stepMany(steps - 1)
      case _ => ps
    }
  
  object Program:
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
      case true => k
      // 0 == [library] [z] onto the stack
      case false => library + Quoted(z)
    }
    def fromValidBin(bin: String): Program = parse(BitVector.fromValidBin(bin))
    def apply(bin: String): Program = fromValidBin(bin)
