package models

import parsers._

sealed trait Argument
case class SimpleArgument(id: String) extends Argument
case class AuxArgument(id: String) extends Argument
case class OptArgument(id: String) extends Argument

case class Routine(
                    id: Id,
                    arguments: Seq[Argument] = Nil,
                    instructions: Seq[Instruction] = Nil
                  ) extends HasId {

  def withArgument(argument: Argument) = copy(arguments = arguments :+ argument)

  def withInstruction(instruction: Instruction) = copy(instructions = instructions :+ instruction)
}

object Routine {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Routine](zero(ROUTINE, Routine(_)))(Seq(
    // instructions
    { case (routine, line @ Node(Leaf(OpCode(_)) :: _, Angle)) =>
        routine.withInstruction(Instruction.parser.parse(line))
    },
    // argument list
    { case (routine, Node(arguments, Round)) => arguments match {
      case Nil => routine
      case args :+ l => (l +: args).zip(arguments).foldLeft(routine) {
        case (r, (_, Leaf(arg))) if arg == "AUX" || arg == "OPT" => r
        case (r, (Leaf("AUX"), Leaf(arg))) => r.withArgument(AuxArgument(arg))
        case (r, (Leaf("OPT"), Leaf(arg))) => r.withArgument(OptArgument(arg))
        case (r, (Leaf(_), Leaf(arg))) => r.withArgument(SimpleArgument(arg))
        case _ => throw new IllegalArgumentException(s"TODO")
      }
    }
    }
  ))
}

//arguments..zip(arguments.drop(1)).foldLeft(routine) {
//  case (r, Leaf(argument)) => r.withArgument(SimpleArgument(argument))
//
//} }