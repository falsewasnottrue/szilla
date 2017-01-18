package interpreter

import models.{Routine, Value, Variable}

sealed trait InstructionPointer
case class Ip(routine: Routine, line: Int) extends InstructionPointer
case object NoIp extends InstructionPointer

object Global {
  private val routines = scala.collection.mutable.Map[String, Routine]()
  private val globalVariables = scala.collection.mutable.Map[Variable, Value]()

  def set(variable: Variable, value: Value): Unit = globalVariables.put(variable, value)

  def get(variable: Variable): Value = globalVariables.get(variable) match {
    case Some(value) => value
    case None => throw new IllegalStateException(s"no value bound to variable $variable")
  }

  def registerRoutine(routine: Routine): Unit = {
    routines.put(routine.id, routine)
  }

  def loadRoutine(routineName: String): Routine = routines.get(routineName) match {
    case Some(routine) => routine
    case None => throw new IllegalStateException(s"no routine registered for name $routineName")
  }

  // TODO rooms, objecs
}

case class Context(ip: InstructionPointer = NoIp, parent: Option[Context] = None) {

  private val localVariables = scala.collection.mutable.Map[Variable, Value]()
  private val stack = scala.collection.mutable.Stack[Value]()

  def get(variable: Variable): Value = (localVariables.get(variable), parent) match {
    case (Some(value), _) => value
    case (None, Some(p)) => p.get(variable)
    case (None, None) => Global.get(variable)
  }

  def set(variable: Variable, value: Value): Context = {
    localVariables.put(variable, value)
    this
  }

  def setGlobal(variable: Variable, value: Value): Context = {
    Global.set(variable, value)
    this
  }

  def push(value: Value): Context = {
    stack.push(value)
    this
  }

  def pop: Option[Value] = if (stack.isEmpty) None else Some(stack.pop)

  def in: String = "TODO"

  def out(s: String): Unit = print(s)

}
