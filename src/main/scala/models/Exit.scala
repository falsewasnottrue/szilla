package models

sealed trait Exit

case object NoExit extends Exit

// Unconditional exit
case class UExit(roomId: Id) extends Exit
// Conditional exit
case class CExit(roomId: Id) extends Exit
// Function exit
case class FExit(exitId: Id) extends Exit