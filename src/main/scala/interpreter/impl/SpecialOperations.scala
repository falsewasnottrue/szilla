package interpreter.impl

import interpreter._
import models._

import scala.concurrent.BlockContext

object TellInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val args = arguments(ctx)(instruction, ValueTypes.arbitrary)
    args.foreach(value => ctx.out(value.toString))
    ctx
  }
}

object ConstantInterpreter extends BaseInterpreter {
  // defines a constant
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    Global.define(GlobalVariable(name), value)
    ctx
  }
}

object SetGInterpreter extends BaseInterpreter {
  // sets a global variable
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    ctx.setGlobal(GlobalVariable(name), value)
  }
}

object SetInterpreter extends BaseInterpreter {
  // sets a local variable
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    ctx.set(LocalVariable(name), value)
  }
}

object CondInterpreter extends BaseInterpreter {
  // chooses the first condition that is true and runs the code block
  override def apply(ctx: Context)(i: Instruction): Context = {
    val conditions = i.operands.collect { case cond: Condition => cond }
    val condMet = conditions.find { c => Interpreter.evaluate(ctx)(c.cond).pop.contains(BoolValue(true))}

    condMet match {
      case Some(condition) => new Context(BlockIp(condition.block), Some(ctx))
      case _ => ctx // no condition was met
    }
  }
}

object RepeatInterpreter extends BaseInterpreter {
  // repeats the inner block
  override def apply(ctx: Context)(i: Instruction): Context = {
    val block = new Block(i.operands.collect { case instruction: Instruction => instruction })
    val inner = new Context(BlockIp(block, repeating = true), Some(ctx))

    inner
  }
  /*
  {
    // FIXME implement
    val inner = new Context(ip = null, parent = Some(ctx))
    def run(c: Context, block: Seq[Operand]): Context = block match {
      case Nil => run(c, i.operands) // start over
      case (instruction: Instruction) :: _ if instruction.opCode == RETURN => c
      case current :: rest => run(Interpreter.evaluate(c)(current), rest)
    }

    run(inner, i.operands)
  }
  */
}