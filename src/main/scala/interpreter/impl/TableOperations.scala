package interpreter.impl
import interpreter.Context
import models.{Instruction, TableValue}

object TableInterpreter extends BaseInterpreter {
  // creates a table on the stack
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    val map = args.zipWithIndex.map { case (value, index) => (index, value) }.toMap
    ctx.push(TableValue(map))
  }
}

// TODO GET
// TODO PUT
// TODO NTBL_Q
// TODO COPYT
