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
    val Seq(RefValue(id), StringValue(flagId)) = arguments(ctx)(i, ValueTypes(RefType, StringType))
    val Some(obj: Object) = ctx.deref(id)
    ctx.push(BoolValue(obj.flags.contains(Flag(flagId))))
  }
}

object InQInterpreter extends BaseInterpreter {
  // Returns true if object2 is the LOC of object1.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(id1), RefValue(id2)) = arguments(ctx)(i, ValueTypes(RefType, RefType))
    val res = (ctx.deref(id1), ctx.deref(id2)) match {
      case (Some(obj1: Object), Some(obj2: Object)) => obj2.contains(obj1)
      case (Some(obj1: Object), Some(room: Room)) => room.contains(obj1)
      case (o1, o2) => throw new IllegalStateException(s"$o2 cannot be the loc of $o1")
    }
    ctx.push(BoolValue(res))
  }
}

object AndInterpreter extends BaseInterpreter {
  // Calculates conjunction of values
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.continually(BoolType))
    val result = args.foldLeft(true) {
      case (false, _) => false
      case (true, BoolValue(b)) => b
      case (_, v) => throw new IllegalArgumentException(s"unexpected value in AND $v")
    }
    ctx.push(BoolValue(result))
  }
}

object OrInterpreter extends BaseInterpreter {
  // Calculates disjunction of values
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes.continually(BoolType))
    val result = args.foldLeft(false) {
      case (true, _) => true
      case (false, BoolValue(b)) => b
      case (_, v) => throw new IllegalArgumentException(s"unexpected value in OR $v")
    }
    ctx.push(BoolValue(result))
  }
}

object NotInterpreter extends BaseInterpreter {
  // Calculates negation
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(BoolValue(arg)) = arguments(ctx)(i, ValueTypes(BoolType))
    ctx.push(BoolValue(!arg))
  }
}