package models

sealed trait Location

case object Empty extends Location
case object Rooms extends Location
case object Player extends Location
case class RefLocation(id: Id) extends Location