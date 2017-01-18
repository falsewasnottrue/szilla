package interpreter.impl

import interpreter.{Context, Interpreter}
import models._

sealed trait ValueTypes {
  def finite: Boolean
  def size: Int
  def nth(n: Int): ValueType
}

case class SeqValueType(seq: Seq[ValueType]) extends ValueTypes {
  override val finite = true
  override val size = seq.size
  override def nth(n: Int) = seq(n)
}

case class StreamValueType(stream: Stream[ValueType]) extends ValueTypes {
  override val finite = false
  override def size = throw new IllegalStateException("cannot get size of infinite stream")
  override def nth(n: Int) = stream(n)
}

object ValueTypes {
  def apply(valueTypes: ValueType*) = fromSeq(valueTypes)

  def continually(valueType: ValueType) = fromStream(Stream.continually(valueType))
  val arbitrary = continually(WildcardType)

  val empty = fromSeq(Nil)

  def fromSeq(seq: Seq[ValueType]) = SeqValueType(seq)
  def fromStream(stream: Stream[ValueType]) = StreamValueType(stream)
}

abstract class BaseInterpreter {

  def arguments(ctx: Context)(instruction: Instruction, expected:ValueTypes): Seq[Value] = {
    if (expected.finite && expected.size != instruction.operands.size) {
      throw new IllegalArgumentException(s"${instruction.opCode} expects ${expected.size} arguments")
    }
    instruction.operands.foldLeft((Seq[Value](), 0)) {
      case ((as, pos), operand) => {
        val e: ValueType = expected.nth(pos)
        Interpreter.evaluate(ctx)(operand)
        ctx.pop match {
          case Some(value) if e == WildcardType => (as :+ value, pos+1)
          case Some(i @ IntValue(_)) if e == IntType => (as :+ i, pos+1)
          case Some(b @ BoolValue(_)) if e == BoolType => (as :+ b, pos+1)
          case Some(s @ StringValue(_)) if e == StringType => (as :+ s, pos+1)
          case Some(o @ ObjectValue(_)) if e == ObjectType => (as :+ o, pos+1)

          case x => throw new IllegalArgumentException(s"unexpected argument in ${instruction.opCode}: $x is not $e")
        }
      }
    }._1
  }

  def apply(ctx: Context)(i: Instruction): Context
}
