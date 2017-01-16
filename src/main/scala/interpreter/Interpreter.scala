package interpreter

import interpreter.impl.AddInterpreter
import models._

object Interpreter {

  private val interpreters = Map[OpCode, Interpreter] (
    ADD -> AddInterpreter
  )

  def evaluate(ctx: Context)(op: Operand): Context = op match {
    case v @ Variable(_) => evaluateVariable(ctx)(v)
    case i @ Instruction(_, _) => evaluateInstruction(ctx)(i)
  }

  private def evaluateVariable(ctx: Context)(v: Variable): Context = v match {
    case Variable(Int(i)) => ctx.push(IntValue(i))
    case Variable(Double(d)) => ctx.push(DoubleValue(d))
    case _ => throw new IllegalStateException(s"Not implemented yet $v")
  }

  private def evaluateInstruction(ctx: Context)(i: Instruction): Context =
    interpreters(i.opCode).apply(ctx, i)
}

object Int {
  def unapply(s: String) : Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}

object Double {
  def unapply(s: String) : Option[Double] = try {
    Some(s.toDouble)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}