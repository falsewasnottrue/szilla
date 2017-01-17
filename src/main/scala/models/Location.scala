package models

trait Location

case object Empty extends Location
case object Rooms extends Location
case object Player extends Location

