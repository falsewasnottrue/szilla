package interpreter

import models._
import impl._

object Interpreters {

  val interpreters = Map[OpCode, Interpreter] (
    ADD -> AddInterpreter
  )
}