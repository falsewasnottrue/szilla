package models

import parsers._

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

// output operations
case object PRINT extends OpCode
case object PRINTD extends OpCode
case object PRINTN extends OpCode
case object BUFOUT extends OpCode
case object CRLF extends OpCode
case object HLIGHT extends OpCode
case object COLOR extends OpCode
case object DIROUT extends OpCode
case object DIRIN extends OpCode

// window operations
case object CURSET extends OpCode
case object CURGET extends OpCode
case object SCREEN extends OpCode
case object CLEAR extends OpCode
case object WINPOS extends OpCode
case object WINSIZE extends OpCode
case object WINATTR extends OpCode
case object SPLIT extends OpCode
case object MARGIN extends OpCode
case object SCROLL extends OpCode

// pictures and sounds
case object DISPLAY extends OpCode
case object PICINF extends OpCode
case object SOUND extends OpCode

// control operations
case object CALL extends OpCode
case object RETURN extends OpCode
case object RTRUE extends OpCode
case object RFALSE extends OpCode

// game commands
case object QUIT extends OpCode
case object RESTART extends OpCode
case object VERIFY extends OpCode
case object SAVE extends OpCode
case object RESTORE extends OpCode
case object ISAVE extends OpCode
case object IRESTORE extends OpCode

// special
case object SETG extends OpCode
case object SET extends OpCode
case object COND extends OpCode
case object TELL extends OpCode
case object REPEAT extends OpCode
case object CONSTANT extends OpCode
case object TABLE extends OpCode

object OpCode {
  def unapply(a: Any): Option[OpCode] = a match {
    // arithmetic instructions
    case "+" | "ADD" => Some(ADD)
    case "-" | "SUB" => Some(SUB)
    case "*" | "MUL" => Some(MUL)
    case "/" | "DIV" => Some(DIV)
    case "MOD" => Some(MOD)
    case "RANDOM" => Some(RANDOM)

    // predicate instructions
    case "EQUAL?" => Some(EQUAL_Q)
    case "ZERO?" => Some(ZERO_Q)
    case "L?" | "LESS?" => Some(LESS_Q)
    case "G?" | "GRTR?" => Some(GRTR_Q)
    case "FSET?" => Some(FSET_Q)
    case "IN?" => Some(IN_Q)

    // object operations
    case "MOVE" => Some(MOVE)
    case "REMOVE" => Some(REMOVE)
    case "LOC" => Some(LOC)
    case "FIRST?" => Some(FIRST_Q)
    case "NEXT?" => Some(NEXT_Q)
    case "FSET" => Some(FSET)
    case "FCLEAR" => Some(FCLEAR)
    case "GETP" => Some(GETP)
    case "PUTP" => Some(PUTP)

    // table operations
    case "GET" => Some(GET)
    case "PUT" => Some(PUT)
    case "INTBL?" => Some(INTBL_Q)
    case "COPYT" => Some(COPYT)

    // input operations
    case "READ" => Some(READ)
    case "INPUT" => Some(INPUT)
    case "MOUSE-INFO" => Some(MOUSE_INFO)
    case "MOUSE-LIMIT" => Some(MOUSE_LIMIT)
    case "MENU" => Some(MENU)

    // output operations
    case "PRINT" => Some(PRINT)
    case "PRINTD" => Some(PRINTD)
    case "PRINTN" => Some(PRINTN)
    case "BUFOUT" => Some(BUFOUT)
    case "CRLF" => Some(CRLF)
    case "HLIGHT" => Some(HLIGHT)
    case "COLOR" => Some(COLOR)
    case "DIROUT" => Some(DIROUT)
    case "DIRIN" => Some(DIRIN)

    // window operations
    case "CURSET" => Some(CURSET)
    case "CURGET" => Some(CURGET)
    case "SCREEN" => Some(SCREEN)
    case "CLEAR" => Some(CLEAR)
    case "WINPOS" => Some(WINPOS)
    case "WINSIZE" => Some(WINSIZE)
    case "WINATTR" => Some(WINATTR)
    case "SPLIT" => Some(SPLIT)
    case "MARGIN" => Some(MARGIN)
    case "SCROLL" => Some(SCROLL)

    // pictures and sounds
    case "DISPLAY" => Some(DISPLAY)
    case "PICINF" => Some(PICINF)
    case "SOUND" => Some(SOUND)

    // control operations
    case "CALL" => Some(CALL)
    case "RETURN" => Some(RETURN)
    case "RTRUE" => Some(RTRUE)
    case "RFALSE" => Some(RFALSE)

    // game commands
    case "QUIT" => Some(QUIT)
    case "RESTART" => Some(RESTART)
    case "VERIFY" => Some(VERIFY)
    case "SAVE" => Some(SAVE)
    case "RESTORE" => Some(RESTORE)
    case "ISAVE" => Some(ISAVE)
    case "IRESTORE" => Some(IRESTORE)

    // special
    case "SET" => Some(SET)
    case "SETG" => Some(SETG)
    case "COND" => Some(COND)
    case "TELL" => Some(TELL)
    case "REPEAT" => Some(REPEAT)
    // TODO GLOBAL
    // TODO QUEUE
    case "CONSTANT" => Some(CONSTANT)
    case "TABLE" => Some(TABLE)

    case _ => None
  }
}

sealed trait Operand

case class Variable(name: String) extends Operand
object GlobalVariable {
  def unapply(s: String): Option[String] =
    if (s.startsWith(",")) Some(s.substring(1)) else None
}
object PropertyNameVariable {
  def unapply(s: String): Option[String] =
    if (s.startsWith(",P?")) Some(s.substring(3)) else None
}

case class Condition(cond: Operand, action: Operand*) extends Operand

case class Instruction(opCode: OpCode, operands: Seq[Operand] = Nil) extends Operand {
  def withOperand(operand: Operand) = copy(operands = operands :+ operand)
}

object Instruction {
  import parsers.ZParser

  def parser: ZParser[Instruction] = {
    val init: PartialFunction[Tree,(Instruction, Seq[Tree])] =
    {
      // TODO Routine CALL (-> Instruction)
      // TODO args
      case Node(Leaf("REPEAT") :: Node(args, Round) :: clauses, Angle) => (Instruction(REPEAT), clauses)
      case Node(Leaf(OpCode(opCode)) :: clauses, Angle) => (Instruction(opCode), clauses)
    }
    val clauseParsers: Seq[PartialFunction[(Instruction, Tree), Instruction]] = Seq(
      { case (i, Leaf(varName)) => i.withOperand(Variable(varName)) },
      { case (i, line @ Node(Leaf(OpCode(_)) :: _, Angle)) => i.withOperand(Instruction.parser.parse(line)) },
      // cond 1
      { case (i, Node(Leaf(predicate) :: actions, Round)) =>
        i.withOperand(Condition(
          Variable(predicate),
          actions.map(Instruction.parser.parse):_*
        )) },
      // cond n
      { case (i, Node(predicate :: actions, Round)) =>
        i.withOperand(Condition(
          Instruction.parser.parse(predicate),
          actions.map(Instruction.parser.parse):_*
        )) }
    )

    ZParser[Instruction](init)(clauseParsers)
  }
}