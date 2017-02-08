package models.syntax

case class ObjectToken(syntaxTokens: Seq[SyntaxToken]) {
  def withSyntaxToken(syntaxToken: SyntaxToken) = copy(syntaxTokens = syntaxTokens :+ syntaxToken)
}

object ObjectToken {
  val KEYWORD = "OBJECT"

  def unapply(s: String): Option[ObjectToken] = s match {
    case `KEYWORD` => Some(ObjectToken(Nil))
    case ObjectTokenString(tokens) => {
      val ts = tokens.split(" ").zip(tokens.split(" ").drop(1) :+ " ").collect {
        case (curr, _) if SyntaxToken.unapply(curr).isDefined => SyntaxToken.unapply(curr).get
        case (curr, next) if SyntaxToken.unapply(s"$curr $next").isDefined => SyntaxToken.unapply(s"$curr $next").get
      }

      Some(ts.foldLeft(ObjectToken(Nil)) {
        case (t, token) => t.withSyntaxToken(token)
      })
    }
    case _ => None
  }
}

object ObjectTokenString {
  def unapply(s: String): Option[String] =
    if (s.startsWith(ObjectToken.KEYWORD + " (") && s.endsWith(")"))
      Some(s.stripPrefix(ObjectToken.KEYWORD + " (").stripSuffix(")"))
    else
      None
}