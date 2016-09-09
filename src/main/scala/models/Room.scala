package models

case class Room(id: Id, desc: Option[String] = None) extends HasId {

  def north: Exit = NExit
  def south: Exit = NExit
  def west: Exit = NExit
  def east: Exit = NExit
  def up: Exit = NExit
  def down: Exit = NExit

  def action: Option[Action] = None
}
