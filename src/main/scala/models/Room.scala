package models

case class Room(id: Id, desc: Option[String] = None, exits: Map[Direction, Exit] = Map.empty) extends HasId with HasLocation {

  def north: Exit = exits.getOrElse(North, NExit)
  def south: Exit = exits.getOrElse(South, NExit)
  def west: Exit = exits.getOrElse(West, NExit)
  def east: Exit = exits.getOrElse(East, NExit)
  def up: Exit = exits.getOrElse(Up, NExit)
  def down: Exit = exits.getOrElse(Down, NExit)

  def action: Option[Action] = None

  val location = Rooms
}