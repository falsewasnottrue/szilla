package models

sealed trait Exit

case object NoExit extends Exit
case class ExitTo(roomId: Id) extends Exit
case class ExitPer(exitId: Id) extends Exit