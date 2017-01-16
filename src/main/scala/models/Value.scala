package models

sealed trait Value
case class IntValue(v: Int) extends Value
case class StringValue(v: Int) extends Value
case class BoolValue(v: Boolean) extends Value