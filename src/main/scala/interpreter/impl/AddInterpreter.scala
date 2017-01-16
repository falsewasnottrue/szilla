package interpreter.impl

import interpreter.{Context, Interpreter}
import models._

object AddInterpreter extends Interpreter {

  override def apply(ctx: Context, ins: Instruction): Context = {
    if (ins.operands.size != 2) {
      throw new IllegalArgumentException(s"wrong arguments count ni $ins")
    }

    ins.operands.foreach(operand => Interpreter.evaluate(ctx)(operand))
    val res = (ctx.pop, ctx.pop) match {
      case (Some(IntValue(i)), Some(IntValue(j))) => IntValue(i+j)
      case (Some(IntValue(i)), Some(DoubleValue(j))) => DoubleValue(i+j)
      case (Some(DoubleValue(i)), Some(IntValue(j))) => DoubleValue(i+j)
      case (Some(DoubleValue(i)), Some(DoubleValue(j))) => DoubleValue(i+j)

      case x => throw new IllegalStateException(s"cannot add $x")
    }
    ctx.push(res)
  }

}
