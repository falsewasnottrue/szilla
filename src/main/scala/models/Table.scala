package models

case class Table(id: Id) extends HasId {

  private val values = scala.collection.mutable.Map[Int, Value]()

  def put(index: Int, value: Value): Unit = ???

  def get(index: Int): Option[Value] = ???
  // <CONSTANT MAZE-TABLE
//  <TABLE 12 18 24 0 0 0>>
}
