package parsers

// Parantheses type
sealed trait ParType
case object Angle extends ParType
case object Round extends ParType
