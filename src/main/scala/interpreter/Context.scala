package interpreter

import models.Variable

class Context(parent: Option[Context]) {

  val global: Context = ???

  def getVar(variable: Variable): Any = ???

  def setVar(variable: Variable, value: Any): Unit = ???
}
