package interpreter.impl
import interpreter.{Context, Global, Ip}
import models._

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration._

// TODO READ

object InputInterpreter extends BaseInterpreter {
  // Input is similar to read, except that it reads a single keystroke, rather than a line
  // of text. The argument is a number corresponding to an input device; as of now,
  // the only input device defined is the keyboard, with number 1. The first optional
  // argument tells INPUT, rather than waiting forever for a keystroke, to wait only
  // that many tenths of a second. The second optional argument is the name of a
  // routine that INPUT should call if it "times out"—that is, if it gets no keystroke
  // within the allotted time. Example:
  override def apply(ctx: Context)(i: Instruction): Context = {
    val args = arguments(ctx)(i, ValueTypes(IntType, Optional(IntType), Optional(StringType)))
    if (args.length != 1 && args.length != 3) {
      throw new IllegalArgumentException(s"illegal arguments for INPUT: $args")
    }

    val IntValue(d) = if (args.length == 3) args(1) else IntValue(100)
    val maybeRoutine =  if (args.length == 3) Some(args(2).toString) else None
    try {
      val key = Await.result[String](ctx.in, (d/10) second)
      ctx.push(StringValue(key))
    } catch {
      case e: TimeoutException =>
        if (maybeRoutine.isDefined) {
          val routineName = maybeRoutine.get
          val Some(routine) = Global.loadRoutine(routineName)
          val instructionPointer = Ip(routine, 0)
          Context(instructionPointer, Some(ctx))
        } else {
          ctx.push(StringValue(""))
        }
    }
  }
}


// TODO MOUSE_INFO
// TODO MOUSE_LIMIT
// TODO MENU