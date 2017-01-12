package parsers

sealed trait ParType
case object Angle extends ParType
case object Round extends ParType

sealed trait Tree
case class Leaf(value: String) extends Tree {}
case class Node(nodes: Seq[Tree], parType: ParType = Angle) extends Tree

object Parser {
  def parse(tokens: Seq[String]): Tree = {
    def notOpenAngle: Tree => Boolean = _ != Leaf("<")
    def notOpenRound: Tree => Boolean = _ != Leaf("(")

    val stack: Seq[Tree] = tokens.foldLeft(Seq[Tree]()) {
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
