package interpreter.impl

import interpreter.{Context, Global, InstructionPointer, Interpreter}
import models._
import org.scalatest.{FlatSpec, Matchers}

class BaseInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val fakeRoutine = Routine("fake")
    val ctx = new Context(InstructionPointer(fakeRoutine, 0))
    Global.reset()

    def run(ctx: Context)(text: String): Context = {
      val instruction = Instruction.parser.parse(text)
      Interpreter.evaluate(ctx)(instruction)
    }

    def createObject(name: String,
                     properties: Properties = Properties()): Object = {
      val variable = GlobalVariable(name)
      val obj = Object(id = name, properties = properties)
      Global.registerObject(obj)
      Global.define(variable, RefValue(obj.id))
      obj
    }

    def createRoom(name: String,
                   properties: Properties = Properties()): Room = {
      val variable = GlobalVariable(name)
      val room = Room(id = name, properties = properties)
      Global.registerRoom(room)
      Global.define(variable, RefValue(room.id))
      room
    }

    def createFlag(name: String): Flag = {
      val variable = GlobalVariable(name)
      val flag = Flag(id = name)
      Global.registerFlag(flag)
      Global.define(variable, RefValue(flag.id))
      flag
    }
  }
}
