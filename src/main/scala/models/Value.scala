package models

sealed trait _Type
case object IntType extends _Type
case object StringType extends _Type
case object BoolType extends _Type
// ...

case class Value(_type: _Type, v: Any) {

  def intValue = if (_type == IntType) v.asInstanceOf[Int] else throw new IllegalArgumentException

}
