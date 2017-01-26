package models

sealed trait Exit

case object NoExit extends Exit

// Unconditional exit
case class UExit(roomId: Id) extends Exit
// Conditional exit
case class CExit(roomId: Id, condition: Variable, otherwise: String) extends Exit
// Function exit
case class FExit(exitId: Id) extends Exit

// TODO door exit
// case class DExit (door exit) extends Exit