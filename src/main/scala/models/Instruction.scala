package models

import parsers.{Leaf, Node, Tree}

sealed trait OpCode

// arithmetic instructions
case object ADD extends OpCode
case object SUB extends OpCode
case object MUL extends OpCode
case object DIV extends OpCode
case object MOD extends OpCode
case object RANDOM extends OpCode

// predicate instructions
case object EQUAL extends OpCode
case object ZERO extends OpCode
case object LESS extends OpCode
case object GRTR extends OpCode
case object FSET extends OpCode
case object IN extends OpCode

// TODO object operations
// TODO table operations
// TODO input operations
// TODO output operations
// TODO window operations
// TODO pictures and sounds
// TODO control operations
// TODO game commands

object OpCode {
  def unapply(a: Any): Option[OpCode] = a match {
    case "+" | "ADD" => Some(ADD)
    case "-" | "SUB" => Some(SUB)
    case "*" | "MUL" => Some(MUL)
    case "/" | "DIV" => Some(DIV)
    case "MOD" => Some(MOD)
    case "RANDOM" => Some(RANDOM)

    case "EQUAL?" => Some(EQUAL)
    case "ZERO?" => Some(ZERO)
    case "L?" | "LESS?" => Some(LESS)
    case "G?" | "GRTR?" => Some(GRTR)
    case "FSET?" => Some(FSET)
    case "IN?" => Some(IN)

    case _ => None
  }
}

case class Instruction(opcode: OpCode, operands: Seq[String] = Nil) {
  def withOperand(operand: String) = copy(operands = operands :+ operand)
}

object Instruction {
  import parsers.ZParser

  def parser = {
    val init: PartialFunction[Tree,(Instruction, Seq[Tree])] =
    { case Node(Leaf(OpCode(opCode)) :: clauses) => (Instruction(opCode), clauses) }
    val clauseParsers: Seq[PartialFunction[(Instruction, Tree), Instruction]] = Seq(
      { case (i, Leaf(op)) => i.withOperand(op) }
    )

    ZParser[Instruction](init)(clauseParsers)
  }
}