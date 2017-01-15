package interpreter

import models.{Value, Variable}

case class Context(parent: Option[Context] = None, values: Map[Variable, Value] = Map[Variable, Value]()) {

  def get(variable: Variable): Option[Value] = values.get(variable)

  def set(variable: Variable, value: Value): Context = copy(parent = Some(this), values = values + (variable -> value))

  def in: String = "TODO"

  def out(s: String): Unit = println(s)

}
