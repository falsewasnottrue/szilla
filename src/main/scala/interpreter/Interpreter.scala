package interpreter

import interpreter.impl._
import models._

object Interpreter {

  private val interpreters = Map[OpCode, Interpreter] (
    ADD -> AddInterpreter,
    TELL -> TellInterpreter
  )

  def evaluate(ctx: Context)(op: Operand): Context = op match {
    case v @ Variable(_) => evaluateVariable(ctx)(v)
    case i @ Instruction(_, _) => evaluateInstruction(ctx)(i)
  }

  private def evaluateVariable(ctx: Context)(v: Variable): Context = v match {
    case Variable(Int(i)) => ctx.push(IntValue(i))
    case Variable(Double(d)) => ctx.push(DoubleValue(d))
    case Variable(Global(g)) => ctx.get(v) match {
      case Some(value) => ctx.push(value)
      case _ => throw new IllegalStateException(s"variable not set $v")
    }
    case Variable(s) => ctx.push(StringValue(s))

    case _ => throw new IllegalStateException(s"not implemented $v")
  }

  private def evaluateInstruction(ctx: Context)(i: Instruction): Context =
    interpreters(i.opCode).apply(ctx, i)
}