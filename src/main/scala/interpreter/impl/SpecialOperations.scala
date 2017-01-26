package interpreter.impl

import interpreter.{Context, Global}
import models._

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
    ctx.setGlobal(GlobalVariable(name), value)
    ctx
  }
}

object SetGInterpreter extends BaseInterpreter {
  // sets a global variable
  override def apply(ctx: Context)(i: Instruction): Context = {
    ???
  }
}
