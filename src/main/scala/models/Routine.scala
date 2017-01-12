package models

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
    //{ case Node(Leaf(OpCode(opCode)) :: clauses) => Instruction.parser.par }
  ))
}