package interpreter.impl
import interpreter.{Context, Global}
import interpreter.impl.FSetInterpreter.arguments
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

object NextQInterpreter extends BaseInterpreter {
  // Returns the next object in the linked contents of object1's LOC.
  // Returns false if object1 is the "last" object in its LOC.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(id)) = arguments(ctx)(i, ValueTypes(RefType))
    ctx.deref(id) match {
      case Some(obj: HasLocation) => obj.location match {
        case RefLocation(locId) => {
          ctx.deref(locId) match {
            case Some(loc: ContainsObjects) => loc.next(obj) match {
              case Some(nextObj) => ctx.push(RefValue(nextObj.id))
              case _ => ctx.push(BoolValue(false))
            }
            case _ => ctx.push(BoolValue(false))
          }
        }
        case _ => ctx.push(BoolValue(false))
      }
      case _ => throw new IllegalArgumentException(s"cannot deref $id")
    }
    ctx
  }
}

object FSetInterpreter extends BaseInterpreter {
  // Sets flag1 in object1.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(objId), RefValue(flagId)) = arguments(ctx)(i, ValueTypes(RefType, RefType))
    (ctx.deref(objId), ctx.deref(flagId)) match {
      case (Some(hasFlags: HasFlags), Some(flag: Flag)) => hasFlags.addFlag(flag)
      case _ => throw new IllegalArgumentException(s"cannot deref $objId or $flagId")
    }
    ctx
  }
}

object FClearInterpreter extends BaseInterpreter {
  // Clears flag1 in object1.
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(objId), RefValue(flagId)) = arguments(ctx)(i, ValueTypes(RefType, RefType))
    (ctx.deref(objId), ctx.deref(flagId)) match {
      case (Some(hasFlags: HasFlags), Some(flag: Flag)) => hasFlags.removeFlag(flag)
      case _ => throw new IllegalArgumentException(s"cannot deref $objId or $flagId")
    }
    ctx
  }
}

object GetPInterpreter extends BaseInterpreter {
  // returns the specified property of object
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(objId), StringValue(propName)) = arguments(ctx)(i, ValueTypes(RefType, StringType))
    ctx.deref(objId) match {
      case Some(hasProperties: HasProperties) => hasProperties.properties.get(propName) match {
        case Some(value) => ctx.push(StringValue(value))
        case None => ctx.push(BoolValue(false))
      }
      case _ => throw new IllegalArgumentException(s"cannot deref $objId")
    }
    ctx
  }
}

object PutPInterpreter extends BaseInterpreter {
  // changes the value of the given object's given property to thing
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(RefValue(objId), StringValue(propName), newValue) = arguments(ctx)(i, ValueTypes(RefType, StringType, WildcardType))
    ctx.deref(objId) match {
      case Some(obj: Object) => newValue match {
        case StringValue(s) => Global.update(obj.copy(properties = obj.properties.add(propName, s)))
        case IntValue(j) => Global.update(obj.copy(properties = obj.properties.addInt(propName, j)))
        case x => throw new IllegalArgumentException(s"cannot set property value $x")
      }
      case Some(room: Room) => newValue match {
        case StringValue(s) => Global.update(room.copy(properties = room.properties.add(propName, s)))
        case IntValue(j) => Global.update(room.copy(properties = room.properties.addInt(propName, j)))
        case x => throw new IllegalArgumentException(s"cannot set property value $x")
      }
      case _ => throw new IllegalArgumentException(s"cannot deref $objId")
    }
    ctx
  }
}

