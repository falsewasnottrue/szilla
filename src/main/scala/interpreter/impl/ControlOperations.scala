package interpreter.impl

import interpreter.{Context, Global, Ip}
import models._

object CallInterpreter extends BaseInterpreter {
  override def step(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    if (args.length < 1) {
      throw new IllegalArgumentException(s"CALL needs a routine name")
    }
    val StringValue(routineName) = args.head
    val Some(routine) = Global.loadRoutine(routineName)
    val instructionPointer = Ip(routine, 0)
    val context = Context(instructionPointer, Some(ctx))

    val parameterValues = args.drop(1)

    if (parameterValues.length < routine.params.length) {
      throw new IllegalArgumentException(s"too few arguments for routine $routineName")
    }
    if (parameterValues.length > routine.params.length + routine.optParams.length) {
      throw new IllegalArgumentException(s"too many arguments for routine $routineName")
    }

    val initialised = for {
      (value, argument) <- parameterValues.zip(routine.params ++ routine.optParams)
      _ = context.set(LocalVariable(argument.id), value)
    } yield argument.id

    for (argument <- routine.arguments.filter(arg => !initialised.contains(arg.id))) {
      context.set(LocalVariable(argument.id), BoolValue(false))
    }

    context
  }

  // special handling implemented
  override def advance(ctx: Context): Context = ctx
}

object ReturnInterpreter extends BaseInterpreter {
  override def step(ctx: Context)(i: Instruction): Context = {
    if (ctx.parent.isEmpty) {
      throw new IllegalStateException(s"cannot return from top-most context")
    }
    val Seq(result) = arguments(ctx)(i, ValueTypes(WildcardType))
    val Some(outer) = ctx.parent

    outer.push(result)
  }
}

object RTrueInterpreter extends BaseInterpreter {
  override def step(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(true))
  }
}

object RFalseInterpreter extends BaseInterpreter {
  override def step(ctx: Context)(i: Instruction): Context = {
    println(ctx)
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(false))
  }
}