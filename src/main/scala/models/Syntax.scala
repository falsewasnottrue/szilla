package models


case class Syntax(verbId: String, verb: String, obj1: Option[ObjectToken] = None, prep: Option[String] = None, obj2: Option[ObjectToken] = None) {

}

object Syntax {
  val SYNTAX = """<SYNTAX (\S+) (OBJECT)? = (\S+)>""".r
  // val SYNTAX = """<SYNTAX (.+) (OBJECT (\(.*\))?)? (.+)? (OBJECT (\(.*\))?)? = (\S+)>""".r

  def unapply(str: String): Option[Syntax] = str match {
    case SYNTAX(verb, null, verbId) => Some(Syntax(verbId, verb))
    case SYNTAX(verb, obj1, verbId) =>
      Some(Syntax(verbId, verb, ObjectToken.unapply(obj1)))

    case _ => None
  }

}
