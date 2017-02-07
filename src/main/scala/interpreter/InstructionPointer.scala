package interpreter

import models.{HasInstructions, Instruction, Routine}

case class InstructionPointer(is: HasInstructions, var line: Int = -1, repeating: Boolean = false) {
  val scope: Boolean = is.isInstanceOf[Routine]

  def instruction: Option[Instruction] = if (line < 0 || line >= is.instructions.size) None else Some(is.instructions(line))

  def inc: Unit = line = line+ 1

  def reset(l: Int): InstructionPointer = if (!repeating) {
    throw new IllegalStateException(s"cannot reset ip")
  } else {
    line = l; this
  }
}
