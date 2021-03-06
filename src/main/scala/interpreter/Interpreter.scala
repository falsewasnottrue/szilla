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
    AND -> AndInterpreter,
    OR -> OrInterpreter,
    NOT -> NotInterpreter,
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
    AGAIN -> AgainInterpreter,
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
    INPUT -> InputInterpreter,
    SET -> SetInterpreter,
    COND -> CondInterpreter,
    REPEAT -> RepeatInterpreter
  )

  def evaluate(ctx: Context)(op: Evaluable): Context = {

    // println(s"$op")
    // println(s"$op in $ctx")

    op match {
      case v @ LocalVariable(_) => evaluateVariable(ctx)(v)
      case v @ GlobalVariable(_) => evaluateVariable(ctx)(v)
      case v @ PropertyNameVariable(_) => evaluateVariable(ctx)(v)
      case v @ Literal(_) => evaluateVariable(ctx)(v)

      case i @ Instruction(_, _) => evaluateInstruction(ctx)(i)

      case x => throw new IllegalArgumentException(s"unknown operand type $x")
    }
  }

  private def evaluateVariable(ctx: Context)(v: Var): Context = v match {
    case PropertyNameVariable(p) => ctx.push(StringValue(p))
    case Literal(Int(i)) => ctx.push(IntValue(i))
      // constants
    case Literal("T") => ctx.push(BoolValue(true))
    case Literal("CR") => ctx.push(StringValue("\n"))
      // fallback
    case Literal(r) => ctx.push(StringValue(r))
    case l @ LocalVariable(_) if ctx.isDefined(l) => ctx.push(ctx.get(l))
    case g @ GlobalVariable(_) => ctx.push(Global.get(g))

    case _ => throw new IllegalStateException(s"not implemented $v")
  }

  private def evaluateInstruction(ctx: Context)(i: Instruction): Context =
    interpreters(i.opCode)(ctx)(i)

  def run(ctx: Context): Context = ctx.ip.instruction match {
    case None => ctx
    case Some(instruction) => {
      val afterEvalCtx = evaluate(ctx)(instruction)
      val afterAdvanceCtx = advance(afterEvalCtx, instruction)

      run(afterAdvanceCtx)
    }
  }

  private def advance(ctx: Context, after: Instruction): Context = {
    after.opCode match {
      case CALL => ctx // use the context produced
      case _ => {
        ctx.inc
        (ctx.ip.instruction, ctx.parent) match {
          case (Some(_), _) => ctx
          case (None, _) if ctx.ip.repeating => ctx.reset()
          case (None, Some(c)) => advance(c, after)
          case _ => ctx // ???
        }
      }
    }
  }
}