package interpreter.impl

import interpreter.{Context, Global, Interpreter}
import models._
import org.scalatest.{FlatSpec, Matchers}

class BaseInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()

    def run(ctx: Context)(text: String): Context = {
      val instruction = Instruction.parser.parse(text)
      Interpreter.evaluate(ctx)(instruction)
    }

    def createObject(c: Context)(name: String): Object = {
      val variable = Variable("," + name)
      val obj = Object(id = name)
      Global.registerObject(obj)
      c.set(variable, ObjectValue(obj.id))
      obj
    }

    def createRoom(c: Context)(name: String): Room = {
      val variable = Variable("," + name)
      val room = Room(id = name)
      Global.registerRoom(room)
      c.set(variable, ObjectValue(room.id))
      room
    }
  }
}
