package models

import parsers._

sealed trait Argument {
  def id: String
}
case class SimpleArgument(id: String) extends Argument
case class AuxArgument(id: String) extends Argument
case class OptArgument(id: String) extends Argument

case class Routine(
                    id: Id,
                    arguments: Seq[Argument] = Nil,
                    instructions: Seq[Instruction] = Nil
                  ) extends HasId {

  val length = instructions.length

  def params = arguments.collect { case arg @ SimpleArgument(_) => arg }
  def optParams = arguments.collect { case arg @ OptArgument(_) => arg }
  def auxParams = arguments.collect { case arg @ AuxArgument(_) => arg }

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
    { case (routine, Node(arguments, Round)) =>
        arguments.foldLeft((routine, 0 /* 0=None, 1=Aux, 2=Opt*/)) {
          case ((r, _), Leaf("AUX")) => (r, 1)
          case ((r, _), Leaf("OPT")) => (r, 2)
          case ((r,0), Leaf(argument)) => (r.withArgument(SimpleArgument(argument)), 0)
          case ((r,1), Leaf(argument)) => (r.withArgument(AuxArgument(argument)), 1)
          case ((r,2), Leaf(argument)) => (r.withArgument(OptArgument(argument)), 2)
          case _ => throw new IllegalArgumentException(s"unexpected argument $arguments")
        }._1
    }
  ))
}