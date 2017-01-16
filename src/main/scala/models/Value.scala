package models

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

object Int {
  def unapply(s: String) : Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}

object Global {
  def unapply(s: String): Option[String] =
    if (s.startsWith(",")) Some(s.substring(1)) else None
}