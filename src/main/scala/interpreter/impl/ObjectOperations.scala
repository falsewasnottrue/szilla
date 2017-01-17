package interpreter.impl
import interpreter.Context
import models.{Instruction, ObjectType, ObjectValue}

object MoveInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(ObjectValue(o1), ObjectValue(o2)) = arguments(ctx)(instruction, ValueTypes(ObjectType, ObjectType))
    // puts object 1 into object 2
    o2.insert(o1)
    // TODO o2.setLocation(o1)
    ctx
  }
}

// TODO REMOVE
// TODO LOC
// TODO FIRST_Q
// TODO NEXT_Q
// TODO FSET
// TODO FCLEAR
// TODO GETP
// TODO PUTP

