package interpreter.impl

import interpreter.Context
import models.{BoolValue, Instruction, IntType, IntValue}

object EqualQInterpreter extends InsInterpreter {
  // Returns true if arg1 is equal? to any of the subsequent args
  override def apply(ctx: Context)(i: Instruction): Context =  {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    if (args.size < 2) {
      throw new IllegalArgumentException(s"EQUAL? needs at least two arguments")
    }
    val arg1 = args.head
    val res = args.drop(1).foldLeft(false) {
      case (acc, arg) => acc || (arg1 == arg)
    }
    ctx.push(BoolValue(res) )
  }
}

object ZeroQInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(IntValue(value)) = arguments(ctx)(instruction, ValueTypes(IntType))
    ctx.push(BoolValue(value == 0))
  }
}

object LessQInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(arg1), IntValue(arg2)) = arguments(ctx)(i, ValueTypes(IntType, IntType))
    ctx.push(BoolValue(arg1 < arg2))
  }
}

object GrtrQInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(arg1), IntValue(arg2)) = arguments(ctx)(i, ValueTypes(IntType, IntType))
    ctx.push(BoolValue(arg1 > arg2))
  }
}

// TODO FSET_Q
// TODO IN_Q
