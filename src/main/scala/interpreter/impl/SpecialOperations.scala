package interpreter.impl

import interpreter.{Context, Global, Interpreter, NoIp}
import models._

object TellInterpreter extends BaseInterpreter {
  override def step(ctx: Context)(instruction: Instruction): Context = {
    val args = arguments(ctx)(instruction, ValueTypes.arbitrary)
    args.foreach(value => ctx.out(value.toString))
    ctx
  }
}

object ConstantInterpreter extends BaseInterpreter {
  // defines a constant
  override def step(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    Global.define(GlobalVariable(name), value)
    ctx
  }
}

object SetGInterpreter extends BaseInterpreter {
  // sets a global variable
  override def step(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    ctx.setGlobal(GlobalVariable(name), value)
  }
}

object SetInterpreter extends BaseInterpreter {
  // sets a local variable
  override def step(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    ctx.set(LocalVariable(name), value)
  }
}

object CondInterpreter extends BaseInterpreter {
  // chooses the first condition that is true and runs the code block
  override def step(ctx: Context)(i: Instruction): Context = {
    val storeIp = ctx.ip
    ctx.ip = NoIp
    val conditions = i.operands.collect { case cond: Condition => cond }
    val condMet = conditions.find { c => {
      Interpreter.evaluate(ctx)(c.cond)
      ctx.pop.contains(BoolValue(true))
    } }

    condMet match {
      case Some(condition) => condition.action.foldLeft(ctx) {
        case (c, op) => Interpreter.evaluate(c)(op)
      }
      case _ => ctx // no condition was met
    }

    ctx.ip = storeIp
    ctx.inc
  }

  override def advance(ctx: Context): Context = ctx
}

object RepeatInterpreter extends BaseInterpreter {
  // repeats the inner block
  override def step(ctx: Context)(i: Instruction): Context = {
    def run(c: Context, block: Seq[Operand]): Context = block match {
      case Nil => c // actually start over
      case (instruction: Instruction) :: _ if instruction.opCode == RETURN => c
      case current :: rest => run(Interpreter.evaluate(c)(current), rest)
    }

    run(ctx, i.operands)
  }
}