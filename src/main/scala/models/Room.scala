package models

case class Room(id: Id, desc: Option[String] = None,
                exits: Map[Direction, Exit] = Map.empty,
                action: Option[Action] = None)
  extends HasId with HasLocation {

  val location = Rooms

  def north: Exit = exits.getOrElse(North, NoExit)
  def south: Exit = exits.getOrElse(South, NoExit)
  def west: Exit = exits.getOrElse(West, NoExit)
  def east: Exit = exits.getOrElse(East, NoExit)
  def up: Exit = exits.getOrElse(Up, NoExit)
  def down: Exit = exits.getOrElse(Down, NoExit)

  def withDesc(desc: String): Room = copy(desc = Some(desc))

  def withAction(action: Action): Room = copy(action = Some(action))

  def withExit(direction: Direction, exit: Exit): Room =
    copy(exits = exits + (direction -> exit))
}