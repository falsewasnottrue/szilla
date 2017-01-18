package interpreter.impl

import interpreter.Context
import models._

object EqualQInterpreter extends BaseInterpreter {
  // Returns true if arg1 is equal? to any of the subsequent args
  override def apply(ctx: Context)(i: Instruction): Context =  {
    val args = arguments(ctx)(i, ValueTypes.arbitrary)
    if (args.size < 2) {
      throw new IllegalArgumentException(s"EQUAL? needs at least two arguments")
    }
    val arg1 = args.head
    val res = args.drop(1).foldLeft(false) {
      case (acc, arg) => acc || (arg1 == arg)
    }
    ctx.push(BoolValue(res) )
  }
}

object ZeroQInterpreter extends BaseInterpreter {
  // Returns true if the value of arg is zero.
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(IntValue(value)) = arguments(ctx)(instruction, ValueTypes(IntType))
    ctx.push(BoolValue(value == 0))
  }
}

object LessQInterpreter extends BaseInterpreter {
  // Returns true if integer1 is less than integer2.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(arg1), IntValue(arg2)) = arguments(ctx)(i, ValueTypes(IntType, IntType))
    ctx.push(BoolValue(arg1 < arg2))
  }
}

object GrtrQInterpreter extends BaseInterpreter {
  // Returns true if integer1 is greater than integer2.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(arg1), IntValue(arg2)) = arguments(ctx)(i, ValueTypes(IntType, IntType))
    ctx.push(BoolValue(arg1 > arg2))
  }
}

object FSetQInterpreter extends BaseInterpreter {
  // Returns true if flag is set in object
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(ObjectValue(obj), StringValue(flagId)) = arguments(ctx)(i, ValueTypes(ObjectType, StringType))
    ctx.push(BoolValue(obj.flags.contains(Flag(flagId))))
  }
}

object InQInterpreter extends BaseInterpreter {
  // Returns true if object2 is the LOC of object1.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(ObjectValue(obj1), ObjectValue(obj2)) = arguments(ctx)(i, ValueTypes(ObjectType, ObjectType))
    ctx.push(BoolValue(obj2.contains(obj1)))
  }
}