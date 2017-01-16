package interpreter.impl

import interpreter.{Context, Interpreter}
import models._

object AddInterpreter extends Interpreter {

  override def apply(ctx: Context, ins: Instruction): Context = {
    val res = ins.operands.foldLeft(0) {
      case (acc, operand) => {
        Interpreter.evaluate(ctx)(operand)
        ctx.pop match {
          case Some(IntValue(i)) => acc + i
          case x => throw new IllegalArgumentException(s"cannot add $x")
        }
      }
    }

    ctx.push(IntValue(res))
  }

}
