package models

sealed trait SyntaxToken
case object HAVE extends SyntaxToken
case object TAKE extends SyntaxToken
case object MANY extends SyntaxToken
case object EVERYWHERE extends SyntaxToken
case object ADJACENT extends SyntaxToken
case object HELD extends SyntaxToken
case object CARRIED extends SyntaxToken
case object ONGROUND extends SyntaxToken
case object INROOM extends SyntaxToken
case class FIND(flagId: String) extends SyntaxToken

object SyntaxToken {
  def unapply(s: String): Option[SyntaxToken] = s match {
    case "HAVE" => Some(HAVE)
    case "TAKE" => Some(TAKE)
    case "MANY" => Some(MANY)
    case "EVERYWHERE" => Some(EVERYWHERE)
    case "ADJACENT" => Some(ADJACENT)
    case "HELD" => Some(HELD)
    case "CARRIED" => Some(CARRIED)
    case "ON-GROUND" => Some(ONGROUND)
    case "IN-ROOM" => Some(INROOM)
    case FindString(flagId) => Some(FIND(flagId))

    case _ => None
  }
}

private object FindString{
  def unapply(str:String): Option[String]= {
    str match {
      case s if s.startsWith("FIND ") => Some(s.stripPrefix("FIND "))
      case _ => None
    }
  }
}