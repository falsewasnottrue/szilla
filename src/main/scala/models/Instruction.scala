package models

import parsers._

sealed trait Evaluable

sealed trait Var extends Evaluable
case class LocalVariable(name: String) extends Var
case class GlobalVariable(name: String) extends Var
case class PropertyNameVariable(name: String) extends Var
case class Literal(name: String) extends Var

object Variable {
  def apply(varname: String): Var =
    if (varname.startsWith(",P?")) PropertyNameVariable(varname.substring(3))
    else if (varname.startsWith(",")) GlobalVariable(varname.substring(1))
    else if (varname.startsWith(".")) LocalVariable(varname.substring(1))
    else Literal(varname)
}

case class Block(instructions: Seq[Instruction] = Nil) extends Evaluable with HasInstructions {
  def withInstruction(instruction: Instruction) = copy(instructions = instructions :+ instruction)
  val length = instructions.size
}

case class Condition(cond: Evaluable, block: Block) extends Evaluable

case class Instruction(opCode: OpCode, operands: Seq[Evaluable] = Nil) extends Evaluable {
  def withOperand(operand: Evaluable) = copy(operands = operands :+ operand)

  override def toString = opCode + "(" + operands.map(_.toString).mkString(",") + ")"
}

object Instruction {
  import parsers.ZParser

  def parser: ZParser[Instruction] = {
    val init: PartialFunction[ParseTree,(Instruction, Seq[ParseTree])] =
    {
      // TODO Routine CALL (-> Instruction)
      // TODO args
      case Node(Leaf("REPEAT") :: Node(args, Round) :: clauses, Angle) => (Instruction(REPEAT), clauses)
      case Node(Leaf(OpCode(opCode)) :: clauses, Angle) => (Instruction(opCode), clauses)
    }
    val clauseParsers: Seq[PartialFunction[(Instruction, ParseTree), Instruction]] = Seq(
      { case (i, Leaf(varName)) => i.withOperand(Variable(varName)) },
      { case (i, line @ Node(Leaf(OpCode(_)) :: _, Angle)) => i.withOperand(Instruction.parser.parse(line)) },
      // cond 1
      { case (i, Node(Leaf(predicate) :: actions, Round)) =>
        i.withOperand(Condition(
          Variable(predicate),
          actions.foldLeft(Block()) {
            case (block, tree) => block.withInstruction(Instruction.parser.parse(tree))
          }
        )) },
      // cond n
      { case (i, Node(predicate :: actions, Round)) =>
        i.withOperand(Condition(
          Instruction.parser.parse(predicate),
          actions.foldLeft(Block()) {
            case (block, tree) => block.withInstruction(Instruction.parser.parse(tree))
          }
        )) }
    )

    ZParser[Instruction](init)(clauseParsers)
  }
}