package interpreter.impl
import interpreter.Context
import models.Instruction

// TODO PRINT
// TODO PRINTD
// TODO PRINTN
// TODO BUFOUT
// TODO HLIGHT
// TODO COLOR
// TODO DIROUT
// TODO DIRIN

object CRLFInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.out("\n")
    ctx
  }
}