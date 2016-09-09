package models

case class Room(id: Id, desc: Option[String] = None) {

  def north: Exit = NExit
  def south: Exit = NExit
  def west: Exit = NExit
  def east: Exit = NExit
  def up: Exit = NExit
  def down: Exit = NExit
}
