package interpreter.impl

import interpreter.{Context, Interpreter}
import models.Instruction

object AddInterpreter extends Interpreter {

  override def apply(ctx: Context, ins: Instruction): Context = ctx

}
