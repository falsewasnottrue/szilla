package interpreter.impl

import interpreter.Context
import models.{BoolValue, Instruction, IntType, IntValue}

/*
case object EQUAL_Q extends OpCode
case object ZERO_Q extends OpCode
case object LESS_Q extends OpCode
case object GRTR_Q extends OpCode
case object FSET_Q extends OpCode
case object IN_Q extends OpCode
 */

object ZeroQInterpreter extends InsInterpreter {

  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(IntValue(value)) = arguments(ctx)(instruction, ValueTypes(IntType))
    ctx.push(BoolValue(value == 0))
  }
}

