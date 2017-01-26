package interpreter

import interpreter.impl._
import models._

object Interpreter {

  private val interpreters = Map[OpCode, BaseInterpreter] (
    ADD -> AddInterpreter,
    SUB -> SubInterpreter,
    MUL -> MulInterpreter,
    DIV -> DivInterpreter,
    MOD -> ModInterpreter,
    RANDOM -> RandomInterpreter,
    EQUAL_Q -> EqualQInterpreter,
    ZERO_Q -> ZeroQInterpreter,
    LESS_Q -> LessQInterpreter,
    GRTR_Q -> GrtrQInterpreter,
    FSET_Q -> FSetQInterpreter,
    IN_Q -> InQInterpreter,
    MOVE -> MoveInterpreter,
    REMOVE -> RemoveInterpreter,
    LOC -> LocInterpreter,
    FIRST_Q -> FirstQInterpreter,
    NEXT_Q -> NextQInterpreter,
    FSET -> FSetInterpreter,
    FCLEAR -> FClearInterpreter,
    GETP -> GetPInterpreter,
    PUTP -> PutPInterpreter,
    PRINT -> PrintInterpreter,
    PRINTD -> PrintDInterpreter,
    PRINTN -> PrintNInterpreter,
    CRLF -> CRLFInterpreter,
    CALL -> CallInterpreter,
    RETURN -> ReturnInterpreter,
    RTRUE -> RTrueInterpreter,
    RFALSE -> RFalseInterpreter,
    TELL -> TellInterpreter,
    TABLE -> TableInterpreter,
    CONSTANT -> ConstantInterpreter,
    GET -> GetInterpreter,
    PUT -> PutInterpreter,
    INTBL_Q -> IntblQInterpreter,
    COPYT -> CopyTInterpreter,
    SETG -> SetGInterpreter,
    INPUT -> InputInterpreter
  )

  def evaluate(ctx: Context)(op: Operand): Context = op match {
      case v @ LocalVariable(_) => evaluateVariable(ctx)(v)
      case v @ GlobalVariable(_) => evaluateVariable(ctx)(v)
      case v @ PropertyNameVariable(_) => evaluateVariable(ctx)(v)

      case i @ Instruction(_, _) => evaluateInstruction(ctx)(i)

      case x => throw new IllegalArgumentException(s"unknown operand type $x")
    }

  private def evaluateVariable(ctx: Context)(v: Var): Context = v match {
    case LocalVariable(Int(i)) => ctx.push(IntValue(i))
    case PropertyNameVariable(p) => ctx.push(StringValue(p))
    case g @ GlobalVariable(_) => ctx.push(Global.get(g))
      // pattern match on string has to come as last default
    case LocalVariable(s) => ctx.push(StringValue(s))

    case _ => throw new IllegalStateException(s"not implemented $v")
  }

  private def evaluateInstruction(ctx: Context)(i: Instruction): Context =
    interpreters(i.opCode)(ctx)(i)
}