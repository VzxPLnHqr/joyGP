package joygp.wip

import cats.effect._
import scodec.bits.BitVector

sealed trait Program
case class Quoted(p: Program) extends Program
case class Effect(f: ProgramState => IO[ProgramState]) extends Program


final case class ProgramState(
  exec: List[Program] = Nil,
  input: BitVector = BitVector.empty,
  output: BitVector = BitVector.empty
)

val id: Program = Effect(state => IO(state))
val readBit: Program = Effect { 
  state => IO(state).map(
    ps => ps.input.headOption match {
      case None => ps // no-op if input is empty
      // if bit is true we will push a specific Quoted program onto the exec stack
      // here we just push Quoted(id) for demonstration
      case Some(true) => ps.copy(exec = Quoted(id) :: ps.exec, input = ps.input.tail.compact)
      case Some(false) => ps.copy(exec = Quoted(id) :: ps.exec, input = ps.input.tail.compact)
    }
  )
}

object Program:
  // `Program` is a monoid!
  def combine(lhs: Program, rhs: Program):Program = (lhs,rhs) match {
      case (Quoted(b), Quoted(a)) => Effect {
        // quotations just get pushed onto stack
        state => IO(state.copy(exec = Quoted(a) :: Quoted(b) :: state.exec))
      }
      case (Quoted(a), Effect(f)) => Effect {
        // try to execute the effect
        state => f(state.copy(exec = Quoted(a) :: state.exec))
      }
      case (Effect(f), Quoted(a)) => Effect {
        // execute f and then push Quoted(a) onto the stack
          state => 
            f(state).map(afterF => afterF.copy(exec = Quoted(a) :: afterF.exec))
      }
      case (Effect(g), Effect(f)) => Effect {
        // execute f after g
        state => g(state).flatMap(f(_))
      }
  }

  extension(p: Program)
    def apply: ProgramState => IO[ProgramState] = p match {
    // the "effect" of a quoted program is to push itself onto the exec stack
    case qp: Quoted => state => IO(state.copy(exec = qp :: state.exec))
    case Effect(f) => state => f(state)
  }

/** read 64 bits of input. This should be fast, but unfortunately it is slooooow!*/
def doTheTest: IO[ProgramState] = {
  val initPS = ProgramState(input = BitVector.fill(64)(true))
  List.range(0,64).foldLeft(IO(initPS))((accumulatedStateIO, _) => accumulatedStateIO.flatMap(readBit(_)))
}