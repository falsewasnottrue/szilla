package models

sealed trait Direction

case object North extends Direction
case object NW extends Direction
case object West extends Direction
case object SW extends Direction
case object South extends Direction
case object SE extends Direction
case object East extends Direction
case object NE extends Direction
case object Up extends Direction
case object Down extends Direction

object Direction {
  def unapply(name: String): Option[Direction] = name match {
    case "NORTH" => Some(North)
    case "NW" => Some(NW)
    case "WEST" => Some(West)
    case "SW" => Some(SW)
    case "SOUTH" => Some(South)
    case "SE" => Some(SE)
    case "EAST" => Some(East)
    case "NE" => Some(NE)
    case "UP" => Some(Up)
    case "DOWN" => Some(Down)
    case _ => None
  }
}