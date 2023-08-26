package joygp

import cats.effect.IO
import scodec.bits.{ByteVector,BitVector}

object JoyBool:

  type Stack[T] = List[T]

  sealed trait Program {
    def expression: String
    def effect: ProgramState => IO[ProgramState]
    override def toString = expression.size match {
      case s if(s > 50) => expression.take(30) + s"... ${s-30} chars ..."
      case _ => expression
    }
    def apply(ps: ProgramState): IO[ProgramState] = effect(ps)
  }
  case class Quoted(p: Program) extends Program {

    def expression: String = s"[$p]"
    def effect: ProgramState => IO[ProgramState] =
      // the effect of a quotation is to push itself onto the exec stack
      state => IO(state.copy(exec = Quoted(p) :: state.exec))
  }
  case class Effect(expression: String, f: ProgramState => IO[ProgramState]) extends Program {
    def effect = f
  }
  def Effect(name: String)( f: ProgramState => IO[ProgramState]): Effect = Effect(name,f)

  // define how programs combine (it's a monoid!)
  extension (lhs: Program)
    def +(rhs: Program): Program = (lhs,rhs) match {
      case (Quoted(b), Quoted(a)) => Effect(s"[$b] [$a]") {
        // quotations just get pushed onto stack
        state => IO(state.copy(exec = Quoted(a) :: Quoted(b) :: state.exec))
      }
      case (Quoted(a), Effect(exp,f)) => Effect(s"[$a] $exp") {
        // try to execute the effect
        state => f(state.copy(exec = Quoted(a) :: state.exec))
      }
      case (Effect(expg,g), Effect(expf,f)) => Effect(s"$expg $expf") {
        // execute f after g
        // seem to need to lift g into IO to avoid stackoverflow
        // this might be known as trapolining, not sure, but it seems to work
        state => IO(g).flatMap(h => h(state)).flatMap(f(_))
      }
      case (Effect(exp,f), Quoted(a)) => Effect(s"$exp [$a]") {
        // execute f and then push Quoted(a) onto the stack
        state => 
          f(state).map(afterF => afterF.copy(exec = Quoted(a) :: afterF.exec))
      }
    }

  // define elementary programs
  // most of the names taken from: http://tunes.org/~iepos/joy.html#appendix
  val id = Effect("")(state => IO(state))

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
      state.input.headOption match {
        case Some(true) => IO(state.copy(exec = Quoted(k) :: state.exec, input = state.input.drop(1)))
        case Some(false) => IO(state.copy(exec = Quoted(z) :: state.exec, input = state.input.drop(1)))
        case None => IO(state) // input is empty, therefore noop
      }
  }

  val putTrue = Effect("putTrue") {
    state => 
      IO(state.copy(output = state.output.+:(true)))
  }

  val putFalse = Effect("putFalse") {
    state =>
      IO(state.copy(output = state.output.+:(false)))
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
  ) {
    override def toString(): String =
      val stackSize = exec.length
      s"\nProgramState(\n \texec ($stackSize items)= ${exec.mkString.take(100)}... \n \tinput=$input \n \toutput=$output\n)"
  }

  extension(ps: ProgramState)
    def step: IO[ProgramState] = ps.stepMany(1)
    def stepMany(steps: Long): IO[ProgramState] = ps.exec match {
      case Nil => IO(ps) // we must be done, no more state changes
      case op :: tail if(steps > 1) => op.effect(ps.copy(exec = tail)).flatMap(_.stepMany(steps - 1))
      case _ => IO(ps)
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
    def rand(numBytes: Int): Program = parse(ByteVector(scala.util.Random.nextBytes(numBytes)).bits)