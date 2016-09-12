package models

case class Object(id: Id, location: Location) extends HasId with HasLocation
