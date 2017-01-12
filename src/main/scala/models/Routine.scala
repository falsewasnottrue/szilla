package models

import parsers._

case class Routine(
                    id: Id,
                    arguments: Seq[String] = Nil,
                    instructions: Seq[Instruction] = Nil
                  ) extends HasId {

  def withArgument(argument: String) = copy(arguments = arguments :+ argument)

  def withInstruction(instruction: Instruction) = copy(instructions = instructions :+ instruction)
}

object Routine {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Routine](zero(ROUTINE, Routine(_)))(Seq(
    // instructions
    { case (routine, line @ Node(Leaf(OpCode(_)) :: _)) =>
        routine.withInstruction(Instruction.parser.parse(line))
    },
    // TODO cond et al
    // argument list
    { case (routine, Node(arguments)) => arguments.foldLeft(routine) {
      case (r, Leaf(argument)) => r.withArgument(argument)
      case _ => throw new IllegalArgumentException(s"")
    } }
  ))
}