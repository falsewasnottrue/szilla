package interpreter

import models._

sealed trait InstructionPointer
case class Ip(routine: Routine, line: Int) extends InstructionPointer
case object NoIp extends InstructionPointer

object Global {
  private val routines = scala.collection.mutable.Map[String, Routine]()
  private val globalVariables = scala.collection.mutable.Map[GlobalVariable, Value]()
  private val rooms = scala.collection.mutable.Map[Id, Room]()
  private val objects = scala.collection.mutable.Map[Id, Object]()
  private val flags = scala.collection.mutable.Map[Id, Flag]()

  // TODO define

  // TODO test defined
  def set(variable: GlobalVariable, value: Value): Unit = globalVariables.put(variable, value)

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
}

case class Context(ip: InstructionPointer = NoIp, parent: Option[Context] = None) {

  private val localVariables = scala.collection.mutable.Map[LocalVariable, Value]()
  private val stack = scala.collection.mutable.Stack[Value]()

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

  def in: String = "TODO"

  def out(s: String): Context = {
    print(s)
    this
  }
}
