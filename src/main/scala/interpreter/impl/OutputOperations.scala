package interpreter.impl

import interpreter.Context
import models._

object PrintInterpreter extends BaseInterpreter {
  // Prints the given string to the current window.
  override def step(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(s)) = arguments(ctx)(i, ValueTypes(StringType))
    ctx.out(s)
  }
}

object PrintDInterpreter extends BaseInterpreter {
  // Prints the DESC of the given object.
  override def step(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(id)) = arguments(ctx)(i, ValueTypes(RefType))
    ctx.deref(id) match {
      case Some(hasProperties: HasProperties) => {
        val desc = hasProperties.properties.get(PropertyName.DESC).getOrElse("")
        ctx.out(desc)
      }
      case x => throw new IllegalArgumentException(s"cannot PRINTD $x")
    }
  }
}

object PrintNInterpreter extends BaseInterpreter {
  // Prints the given number.
  override def step(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(d)) = arguments(ctx)(i, ValueTypes(IntType))
    ctx.out(d.toString)
  }
}

// TODO BUFOUT
// TODO HLIGHT
// TODO COLOR
// TODO DIROUT
// TODO DIRIN

object CRLFInterpreter extends BaseInterpreter {
  override def step(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.out("\n")
    ctx
  }
}