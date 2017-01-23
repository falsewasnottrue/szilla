package interpreter.impl
import interpreter.Context
import models._

import scala.collection.mutable

object TableInterpreter extends BaseInterpreter {
  // creates a table on the stack
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    val values = args.zipWithIndex.map { case (value, index) => (index, value) }
    val table = mutable.HashMap(values:_*)
    ctx.push(TableValue(table))
  }
}

object GetInterpreter extends BaseInterpreter {
  // Returns the value that is stored in the integer1th slot in the given table
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(TableValue(table), IntValue(index)) = arguments(ctx)(i, ValueTypes(TableType, IntType))
    ctx.push(table.getOrElse(index, BoolValue(false)))
  }
}

object PutInterpreter extends BaseInterpreter {
  // Changes the integer1th slot of the given table to thing
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(TableValue(table), IntValue(index), value: Value) = arguments(ctx)(i, ValueTypes(TableType, IntType, WildcardType))
    table.put(index, value)
    ctx
  }
}

// TODO NTBL_Q
// TODO COPYT
