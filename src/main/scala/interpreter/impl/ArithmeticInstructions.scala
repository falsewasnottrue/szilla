package interpreter.impl

import interpreter.Context
import models._

object AddInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val args = arguments(ctx)(instruction, ValueTypes.continually(IntType))
    val res = args.foldLeft(0) {
      case (acc, IntValue(i)) => acc + i
    }
    ctx.push(IntValue(res))
  }
}

object SubInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(IntValue(i1), IntValue(i2)) = arguments(ctx)(instruction, ValueTypes(IntType, IntType))
    ctx.push(IntValue(i1 - i2))
  }
}

object MulInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val args = arguments(ctx)(instruction, ValueTypes.continually(IntType))
    val res = args.foldLeft(1) {
      case (acc, IntValue(i)) => acc * i
    }
    ctx.push(IntValue(res))
  }
}
// TODO DIV
// TODO MOD

object RandomInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(limit)) = arguments(ctx)(i, ValueTypes(IntType))
    val rnd = (new java.util.Random().nextInt.abs % limit) + 1 // 1 <= r <= limit
    ctx.push(IntValue(rnd))
  }
}