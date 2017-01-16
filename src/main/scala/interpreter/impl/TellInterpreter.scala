package interpreter.impl

import interpreter.{Context, Interpreter}
import models._

object TellInterpreter extends Interpreter {
  override def apply(ctx: Context, i: Instruction): Context = {
    for (elem <- i.operands) {
      Interpreter.evaluate(ctx)(elem)
      ctx.pop match {
        case Some(v) => ctx.out(v.toString)
        case _ => throw new IllegalStateException(s"operand not evaluated $elem")
      }
    }

    ctx
  }
}
