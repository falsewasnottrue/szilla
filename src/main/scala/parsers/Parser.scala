package parsers

object Parser {
  def parse(tokens: Seq[String]): ParseTree = {
    def notOpenAngle: ParseTree => Boolean = _ != Leaf("<")
    def notOpenRound: ParseTree => Boolean = _ != Leaf("(")

    val stack: Seq[ParseTree] = tokens.foldLeft(Seq[ParseTree]()) {
      case (acc, token) => token match {
        case ">" =>
          // acc partitionieren l + "<" + r
          val (l, r) = (acc.takeWhile(notOpenAngle), acc.dropWhile(notOpenAngle).drop(1))
          Node(l.reverse, Angle) +: r
        case ")" =>
          // acc partitionieren l + "(" + r
          val (l, r) = (acc.takeWhile(notOpenRound), acc.dropWhile(notOpenRound).drop(1))
          Node(l.reverse, Round) +: r

        case lit => Leaf(lit) +: acc
      }
    }

    stack match {
      case node :: Nil => node
      case _ => throw new IllegalArgumentException
    }
  }
}
