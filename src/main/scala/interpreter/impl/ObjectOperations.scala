package interpreter.impl
import interpreter.Context
import models._

object MoveInterpreter extends BaseInterpreter {
  // Puts object1 into object2
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(ObjectValue(id1), ObjectValue(id2)) = arguments(ctx)(instruction, ValueTypes(ObjectType, ObjectType))
    (ctx.deref(id1), ctx.deref(id2)) match {
      case (Some(obj1: Object), Some(obj2: Object)) => obj2.insert(obj1)
      case (Some(obj1: Object), Some(room: Room)) => room.insert(obj1)
      case (o1, o2) => throw new IllegalStateException(s"cannot insert $o2 into $o1")
    }
    ctx
  }
}

object RemoveInterpreter extends BaseInterpreter {
  // Removes object, setting its LOC to false.
  override def apply(ctx: Context)(i: Instruction): Context = ???
}

// TODO LOC
// TODO FIRST_Q
// TODO NEXT_Q
// TODO FSET
// TODO FCLEAR
// TODO GETP
// TODO PUTP

