package interpreter

import models.{Value, Variable}

case class Context(parent: Option[Context] = None) {

  private val values = scala.collection.mutable.Map[Variable, Value]()
  private val stack = scala.collection.mutable.Stack[Value]()

  // TODO fallback to parent
  def get(variable: Variable): Option[Value] = values.get(variable)

  def set(variable: Variable, value: Value): Unit = values.put(variable, value)

  def push(value: Value): Context = {
    stack.push(value)
    this
  }

  def pop: Option[Value] = if (stack.isEmpty) None else Some(stack.pop)

  def in: String = "TODO"

  def out(s: String): Unit = println(s)

}
