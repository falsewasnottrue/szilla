package interpreter.impl

import interpreter.{Context, Global, InstructionPointer}
import models._

object CallInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    if (args.length < 1) {
      throw new IllegalArgumentException(s"CALL needs a routine name")
    }
    val StringValue(routineName) = args.head
    val Some(routine) = Global.loadRoutine(routineName)
    val instructionPointer = InstructionPointer(routine, 0)
    val context = new Context(instructionPointer, Some(ctx))

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
}

object ReturnInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes(Optional(WildcardType)))
    val scope = Context.findScope(ctx)
    args match {
      case Seq(result) => scope.parent match {
        case Some(outer) => outer.push(result)
        case _ => throw new IllegalStateException(s"cannot return from top-most context")
      }
      case _ => scope
    }
  }
}

object AgainInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    val c = Context.findRepeatable(ctx)
    c.reset(-1)
  }
}

object RTrueInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(true))
  }
}

object RFalseInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(false))
  }
}