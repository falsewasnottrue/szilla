package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Optional, _}

sealed trait ValueTypes {
  def finite: Boolean
  def size: Int
  def nth(n: Int): ValueType
  def fits(size: Int): Boolean
}

case class SeqValueType(expected: Seq[ValueType]) extends ValueTypes {
  override val finite = true
  override val size = expected.size
  override def nth(n: Int) = expected(n)
  override def fits(size: Int) =expected.size >= size && expected.filterNot(_.isInstanceOf[Optional]).size <= size
}

case class StreamValueType(stream: Stream[ValueType]) extends ValueTypes {
  override val finite = false
  override def size = throw new IllegalStateException("cannot get size of infinite stream")
  override def nth(n: Int) = stream(n)
  override def fits(size: Int) = true
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

  def arguments(ctx: Context)(instruction: Instruction, expected: ValueTypes): Seq[Value] = {
    if (expected.finite && !expected.fits(instruction.operands.size)) {
      throw new IllegalArgumentException(s"${instruction.opCode} expects ${expected.size} arguments")
    }
    instruction.operands.foldLeft((Seq[Value](), 0)) {
      case ((as, pos), operand) => {
        val e: ValueType = expected.nth(pos)
        Interpreter.evaluate(ctx)(operand)
        ctx.pop match {
          case Some(value) if e == WildcardType => (as :+ value, pos+1)
          case Some(value) if e == Optional(WildcardType) => (as :+ value, pos+1)

          case Some(i @ IntValue(_)) if e == IntType => (as :+ i, pos+1)
          case Some(i @ IntValue(_)) if e == Optional(IntType) => (as :+ i, pos+1)

          case Some(b @ BoolValue(_)) if e == BoolType => (as :+ b, pos+1)
          case Some(b @ BoolValue(_)) if e == Optional(BoolType) => (as :+ b, pos+1)

          case Some(s @ StringValue(_)) if e == StringType => (as :+ s, pos+1)
          case Some(s @ StringValue(_)) if e == Optional(StringType) => (as :+ s, pos+1)

          case Some(o @ RefValue(_)) if e == RefType => (as :+ o, pos+1)
          case Some(o @ RefValue(_)) if e == Optional(RefType) => (as :+ o, pos+1)

          case Some(t @ TableValue(_)) if e == TableType => (as :+ t, pos+1)
          case Some(t @ TableValue(_)) if e == Optional(TableType) => (as :+ t, pos+1)

          case x => throw new IllegalArgumentException(s"unexpected argument in ${instruction.opCode}: $x is not $e")
        }
      }
    }._1
  }

  def apply(ctx: Context)(i: Instruction): Context = advance(step(ctx)(i))

  def advance(ctx: Context): Context = ctx.inc

  def step(ctx: Context)(i: Instruction): Context
}
