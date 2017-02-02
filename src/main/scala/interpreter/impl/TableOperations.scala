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

object IntblQInterpreter extends BaseInterpreter {
  // Returns true if thing is found within the given table.
  // The third argument is an integer representing the number of elements in the given table.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(elem, TableValue(table), IntValue(len)) = arguments(ctx)(i, ValueTypes(WildcardType, TableType, IntType))
    ctx.push(BoolValue(table.values.exists(_ == elem)))
  }
}

object CopyTInterpreter extends BaseInterpreter {
  // Copies table1 into table2. The process stops at the integer1th slot number;
  // if you desire, all of table1 doesn't have to be copied to table2.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(TableValue(src), TableValue(dest), IntValue(limit)) = arguments(ctx)(i, ValueTypes(TableType, TableType, IntType))
    src.filter { _._1 < limit}.foreach { case (index, value) => dest.put(index, value) }
    ctx
  }
}