package models

sealed trait Direction {
  def name: String
}

case object North extends Direction {
  val name = "NORTH"
}
case object South extends Direction {
  val name = "SOUTH"
}
case object East extends Direction {
  val name = "EAST"
}
case object West extends Direction {
  val name = "WEST"
}
case object Up extends Direction {
  val name = "UP"
}
case object Down extends Direction {
  val name = "DOWN"
}

object Direction {
  def unapply(name: String): Option[Direction] = name match {
    case North.name => Some(North)
    case South.name => Some(South)
    case West.name => Some(West)
    case East.name => Some(East)
    case Up.name => Some(Up)
    case Down.name => Some(Down)
    case _ => None
  }
}