package models

sealed trait Location

case object Empty extends Location
case object Rooms extends Location
case object Player extends Location
case class RoomLocation(roomId: Id) extends Location
