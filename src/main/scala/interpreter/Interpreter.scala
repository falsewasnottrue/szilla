package interpreter

import interpreter.impl._
import models._

object Interpreter {

  private val interpreters = Map[OpCode, InsInterpreter] (
    ADD -> AddInterpreter,
    RANDOM -> RandomInterpreter,
    ZERO_Q -> ZeroQInterpreter,
    TELL -> TellInterpreter
  )

  def evaluate(ctx: Context)(op: Operand): Context = op match {
    case v @ Variable(_) => evaluateVariable(ctx)(v)
    case i @ Instruction(_, _) => evaluateInstruction(ctx)(i)
    case x => throw new IllegalArgumentException(s"unknown operand type $x")
  }

  private def evaluateVariable(ctx: Context)(v: Variable): Context = v match {
    case Variable(Int(i)) => ctx.push(IntValue(i))
    case Variable(Global(g)) => ctx.get(v) match {
      case Some(value) => ctx.push(value)
      case _ => throw new IllegalStateException(s"variable not set $v")
    }
    case Variable(s) => ctx.push(StringValue(s))

    case _ => throw new IllegalStateException(s"not implemented $v")
  }

  private def evaluateInstruction(ctx: Context)(i: Instruction): Context =
    interpreters(i.opCode).apply(ctx)(i)
}