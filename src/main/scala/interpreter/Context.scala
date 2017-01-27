package interpreter

import models._

import scala.concurrent.Future

sealed trait InstructionPointer {
  def instruction: Option[Instruction]
  def inc: Unit
}

case class Ip(routine: Routine, var line: Int) extends InstructionPointer {
  def instruction = if (line >= routine.length) None else Some(routine.instructions(line))
  def inc = line = line+1
}

case object NoIp extends InstructionPointer {
  val instruction = None
  def inc = {}
}

object Global {

  private val routines = scala.collection.mutable.Map[String, Routine]()
  private val globalVariables = scala.collection.mutable.Map[GlobalVariable, Value]()
  private val rooms = scala.collection.mutable.Map[Id, Room]()
  private val objects = scala.collection.mutable.Map[Id, Object]()
  private val flags = scala.collection.mutable.Map[Id, Flag]()

  def define(variable: GlobalVariable, value: Value): Unit =
    if (globalVariables.get(variable).isEmpty) {
      globalVariables.put(variable, value)
    } else {
      throw new IllegalArgumentException(s"cannot redefine global variable $variable")
    }

  def set(variable: GlobalVariable, value: Value): Unit =
    if (globalVariables.get(variable).isDefined) {
      globalVariables.put(variable, value)
    } else {
      throw new IllegalArgumentException(s"cannot set undefined global variable $variable")
    }

  def get(variable: GlobalVariable): Value = globalVariables.get(variable) match {
    case Some(value) => value
    case None => throw new IllegalStateException(s"no value bound to variable $variable")
  }

  def registerRoutine(routine: Routine): Unit = routines.put(routine.id, routine)

  def loadRoutine(routineName: String): Option[Routine] = routines.get(routineName)

  def registerRoom(room: Room): Unit = rooms.put(room.id, room)

  def loadRoom(roomId: Id): Option[Room] = rooms.get(roomId)

  def registerObject(obj: Object): Unit = objects.put(obj.id, obj)

  def loadObject(objId: Id): Option[Object] = objects.get(objId)

  def registerFlag(flag: Flag): Unit = flags.put(flag.id, flag)

  def loadFlag(flagId: Id): Option[Flag] = flags.get(flagId)

  def update(hasId: HasId): Unit = hasId match {
    case obj: Object => registerObject(obj)
    case room: Room => registerRoom(room)
  }

  def reset() = {
    Seq(routines, globalVariables, rooms, objects, flags).foreach(_.clear())
  }
}

case class Context(ip: InstructionPointer = NoIp, parent: Option[Context] = None) {

  private val localVariables = scala.collection.mutable.Map[LocalVariable, Value]()
  private val stack = scala.collection.mutable.Stack[Value]()

  def isDefined(variable: LocalVariable): Boolean = (localVariables.get(variable), parent) match {
    case (Some(_), _) => true
    case (None, Some(p)) => p.isDefined(variable)
    case _ => false
  }

  def get(variable: LocalVariable): Value = (localVariables.get(variable), parent) match {
    case (Some(value), _) => value
    case (None, Some(p)) => p.get(variable)
    case _ => throw new IllegalArgumentException(s"no value bound to local variable $variable")
  }

  def set(variable: LocalVariable, value: Value): Context = {
    localVariables.put(variable, value)
    this
  }

  def setGlobal(globalVariable: GlobalVariable, value: Value): Context = {
    Global.set(globalVariable, value)
    this
  }

  def getGlobal(globalVariable: GlobalVariable): Value = Global.get(globalVariable)

  def deref(id: Id): Option[HasId] =
    Global.loadObject(id).orElse(Global.loadRoom(id)).orElse(Global.loadFlag(id))

  def push(value: Value): Context = {
    stack.push(value)
    this
  }

  def pop: Option[Value] = if (stack.isEmpty) None else Some(stack.pop)

  def inc: Context = {
    ip.inc
    (ip.instruction, parent) match {
      case (Some(_), _) => this
      case (None, Some(c)) => c.inc
      case _ => this
    }
  }

  // TODO implement
  def in: Future[String] = Future.successful("TODO")

  def out(s: String): Context = {
    if (parent.isDefined) {
      parent.get.out(s)
    } else {
      print(s)
    }

    this
  }
}
