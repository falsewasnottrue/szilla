package models

sealed trait ValueType
case object IntType extends ValueType
case object StringType extends ValueType
case object BoolType extends ValueType
case object ObjectType extends ValueType
case object WildcardType extends ValueType

sealed trait Value
case class IntValue(v: Int) extends Value {
  override def toString = v.toString
}
case class StringValue(v: String) extends Value {
  override def toString = v.toString
}
case class BoolValue(v: Boolean) extends Value {
  override def toString = v.toString
}
case class ObjectValue(v: Object) extends Value {
  override def toString = v.toString
}

object Int {
  def unapply(s: String) : Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}
