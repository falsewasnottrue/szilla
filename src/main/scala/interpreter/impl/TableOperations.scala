package interpreter.impl
import interpreter.Context
import models._

object TableInterpreter extends BaseInterpreter {
  // creates a table on the stack
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    val map = args.zipWithIndex.map { case (value, index) => (index, value) }.toMap
    ctx.push(TableValue(map))
  }
}

object GetInterpreter extends BaseInterpreter {
  // Returns the value that is stored in the integer1th slot in the given table
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(TableValue(table), IntValue(index)) = arguments(ctx)(i, ValueTypes(TableType, IntType))
    ctx.push(table.getOrElse(index, BoolValue(false)))
  }
}

// TODO GET
// TODO PUT
// TODO NTBL_Q
// TODO COPYT
