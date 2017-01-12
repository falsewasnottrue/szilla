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
case object EQUAL_Q extends OpCode
case object ZERO_Q extends OpCode
case object LESS_Q extends OpCode
case object GRTR_Q extends OpCode
case object FSET_Q extends OpCode
case object IN_Q extends OpCode

// object operations
case object MOVE extends OpCode
case object REMOVE extends OpCode
case object LOC extends OpCode
case object FIRST_Q extends OpCode
case object NEXT_Q extends OpCode
case object FSET extends OpCode
case object FCLEAR extends OpCode
case object GETP extends OpCode
case object PUTP extends OpCode

// table operations
case object GET extends OpCode
case object PUT extends OpCode
case object INTBL_Q extends OpCode
case object COPYT extends OpCode

// input operations
case object READ extends OpCode
case object INPUT extends OpCode
case object MOUSE_INFO extends OpCode
case object MOUSE_LIMIT extends OpCode
case object MENU extends OpCode

// TODO output operations
// TODO window operations
// TODO pictures and sounds
// TODO control operations
// TODO game command
case object QUIT extends OpCode
case object RESTART extends OpCode
case object VERIFY extends OpCode
case object SAVE extends OpCode
case object RESTORE extends OpCode
case object ISAVE extends OpCode
case object IRESTORE extends OpCode

object OpCode {
  def unapply(a: Any): Option[OpCode] = a match {
    case "+" | "ADD" => Some(ADD)
    case "-" | "SUB" => Some(SUB)
    case "*" | "MUL" => Some(MUL)
    case "/" | "DIV" => Some(DIV)
    case "MOD" => Some(MOD)
    case "RANDOM" => Some(RANDOM)

    case "EQUAL?" => Some(EQUAL_Q)
    case "ZERO?" => Some(ZERO_Q)
    case "L?" | "LESS?" => Some(LESS_Q)
    case "G?" | "GRTR?" => Some(GRTR_Q)
    case "FSET?" => Some(FSET_Q)
    case "IN?" => Some(IN_Q)

    case "MOVE" => Some(MOVE)
    case "REMOVE" => Some(REMOVE)
    case "LOC" => Some(LOC)
    case "FIRST?" => Some(FIRST_Q)
    case "NEXT?" => Some(NEXT_Q)
    case "FSET" => Some(FSET)
    case "FCLEAR" => Some(FCLEAR)
    case "GETP" => Some(GETP)
    case "PUTP" => Some(PUTP)

    case "GET" => Some(GET)
    case "PUT" => Some(PUT)
    case "INTBL?" => Some(INTBL_Q)
    case "COPYT" => Some(COPYT)

    case "READ" => Some(READ)
    case "INPUT" => Some(INPUT)
    case "MOUSE-INFO" => Some(MOUSE_INFO)
    case "MOUSE-LIMIT" => Some(MOUSE_LIMIT)
    case "MENU" => Some(MENU)

    case "QUIT" => Some(QUIT)
    case "RESTART" => Some(RESTART)
    case "VERIFY" => Some(VERIFY)
    case "SAVE" => Some(SAVE)
    case "RESTORE" => Some(RESTORE)
    case "ISAVE" => Some(ISAVE)
    case "IRESTORE" => Some(IRESTORE)

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