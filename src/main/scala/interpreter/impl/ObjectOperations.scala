package interpreter.impl
import interpreter.Context
import models._

object MoveInterpreter extends BaseInterpreter {
  // Puts object1 into object2
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(RefValue(id1), RefValue(id2)) = arguments(ctx)(instruction, ValueTypes(RefType, RefType))
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
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(id)) = arguments(ctx)(i, ValueTypes(RefType))
    val Some(obj: Object) = ctx.deref(id)
    val RefLocation(locId) = obj.location
    val Some(loc: ContainsObjects) = ctx.deref(locId)

    loc.remove(obj)
    ctx
  }
}

object LocInterpreter extends BaseInterpreter {
  // Returns the location of object. Returns false if object has no location.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(id)) = arguments(ctx)(i, ValueTypes(RefType))
    ctx.deref(id) match {
      case Some(hasLocation: HasLocation) => hasLocation.location match {
        case RefLocation(locId) => ctx.push(RefValue(locId))
        case _ => ctx.push(BoolValue(false))
      }
      case _ => throw new IllegalArgumentException(s"cannot deref $id")
    }
    ctx
  }
}

object FirstQInterpreter extends BaseInterpreter {
  // Returns the first object within object1. Returns false if object1 has no contents.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(id)) = arguments(ctx)(i, ValueTypes(RefType))
    ctx.deref(id) match {
      case Some(containsObjects: ContainsObjects) => containsObjects.first match {
        case Some(obj) => ctx.push(RefValue(obj.id))
        case _ => ctx.push(BoolValue(false))
      }
      case _ => throw new IllegalArgumentException(s"cannot deref $id")
    }
    ctx
  }
}

// TODO NEXT_Q
// TODO FSET
// TODO FCLEAR
// TODO GETP
// TODO PUTP

