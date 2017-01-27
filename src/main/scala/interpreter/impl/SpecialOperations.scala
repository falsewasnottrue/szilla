package interpreter.impl

import interpreter.{Context, Global, Interpreter}
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
    println(ctx)
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    ctx.set(LocalVariable(name), value)
  }
}

object CondInterpreter extends BaseInterpreter {
  // chooses the first condition that is true and runs the code block
  override def step(ctx: Context)(i: Instruction): Context = {
    println(s"initCtx: $ctx")
    // TODO save ip
    // val currLine = ctx.ip.line
    val conds = i.operands.collect { case cond: Condition => cond }
    val condMet = conds.find { cond => Interpreter.evaluate(ctx)(cond.cond).pop.contains(BoolValue(true))}

    val resCtx = condMet match {
      case Some(condition) => condition.action.foldLeft(ctx) {
        case (c, op) =>
          println(s"now: $c")
          val x = Interpreter.evaluate(c)(op)
          println(s"and now $x")
          x
      }
      case _ => ctx
    }

    // TODO restore ip
    // resCtx.ip.line = currLine+1

    println(s"resCtx: $resCtx")
    resCtx
  }

  override def advance(ctx: Context): Context = ctx
}