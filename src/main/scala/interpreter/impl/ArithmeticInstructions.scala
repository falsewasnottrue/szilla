package interpreter.impl

import interpreter.Context
import models._

object AddInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    // Adds the given numbers and returns the sum.
    val args = arguments(ctx)(instruction, ValueTypes.continually(IntType))
    val res = args.foldLeft(0) {
      case (acc, IntValue(i)) => acc + i
      case x => throw new IllegalArgumentException(s"unexpected argument for ADD: $x")
    }
    ctx.push(IntValue(res))
  }
}

object SubInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    // Subtracts integer2 from integer1 and returns the difference.
    val Seq(IntValue(i1), IntValue(i2)) = arguments(ctx)(instruction, ValueTypes(IntType, IntType))
    ctx.push(IntValue(i1 - i2))
  }
}

object MulInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    // Multiplies the given numbers and returns the product.
    val args = arguments(ctx)(instruction, ValueTypes.continually(IntType))
    val res = args.foldLeft(1) {
      case (acc, IntValue(i)) => acc * i
      case x => throw new IllegalArgumentException(s"unexpected argument for ADD: $x")

    }
    ctx.push(IntValue(res))
  }
}

object DivInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    // Divides integer1 by integer2 and returns the quotient, truncated to an integer if necessary.
    val Seq(IntValue(i1), IntValue(i2)) = arguments(ctx)(instruction, ValueTypes(IntType, IntType))
    ctx.push(IntValue(i1 / i2))
  }
}

object ModInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    // Divides integer1 by integer2 and returns the remainder.
    val Seq(IntValue(i1), IntValue(i2)) = arguments(ctx)(instruction, ValueTypes(IntType, IntType))
    ctx.push(IntValue(i1 % i2))
  }
}

object RandomInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    // Returns a random number between one and the given number, inclusive.
    val Seq(IntValue(limit)) = arguments(ctx)(i, ValueTypes(IntType))
    val rnd = (new java.util.Random().nextInt.abs % limit) + 1 // 1 <= r <= limit
    ctx.push(IntValue(rnd))
  }
}