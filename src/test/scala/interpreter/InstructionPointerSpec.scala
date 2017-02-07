package interpreter

import models._
import org.scalatest.{FlatSpec, Matchers}

class InstructionPointerSpec extends FlatSpec with Matchers {

  trait Env {
    val instruction1 = Instruction.parser.parse("<SET A 1>")
    val instruction2 = Instruction.parser.parse("<SET B 1>")
    val routine = Routine("fake", instructions = Seq(instruction1, instruction2))
    val block = Block(Seq(instruction1, instruction2))
  }

  "InstructionPointer" should "be constructible around routine" in new Env {
    InstructionPointer(routine).scope should be(true)
  }

  it should "be constructible around a code block" in new Env {
    InstructionPointer(block).scope should be(false)
  }

  it should "be initialised to before start of routine" in new Env {
    val ip = InstructionPointer(routine)
    ip.instruction should be(None)
    ip.inc
    ip.instruction should be(Some(instruction1))
  }

  it should "be initialised to before start of block" in new Env {
    val ip = InstructionPointer(block)
    ip.instruction should be(None)
    ip.inc
    ip.instruction should be(Some(instruction1))
  }

  it should "increase ip in routine with inc" in new Env {
    val ip = InstructionPointer(routine)
    ip.inc
    ip.inc
    ip.instruction should be(Some(instruction2))
    ip.inc
    ip.instruction should be(None)
  }

  it should "increase ip in block with inc" in new Env {
    val ip = InstructionPointer(block)
    ip.inc
    ip.inc
    ip.instruction should be(Some(instruction2))
    ip.inc
    ip.instruction should be(None)
  }

  it should "reset the ip if a block is repeatable" in new Env {
    val ip = InstructionPointer(block, repeating = true)
    ip.inc
    ip.inc
    ip.instruction should be(Some(instruction2))
    ip.reset(0)
    ip.instruction should be(Some(instruction1))
  }

  it should "fail to reset if a block is not repeatable" in new Env {
    val ip = InstructionPointer(routine)
    intercept[IllegalStateException] {
      ip.reset(1)
    }
  }
}
