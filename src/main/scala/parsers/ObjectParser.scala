package parsers

import models._
import KeyWords._

object ObjectParser {
  def parse(text: String): Object = {
    def enrichObj(obj: Object, clause: Tree): Object = clause match {
      case Node(Seq(Leaf(LOC), Leaf(locId))) => obj.withLocation(RoomLocation(locId))
      case Node(Leaf(SYNONYM) +: synonymes) => synonymes.foldLeft(obj) {
        case (o, Leaf(synonym)) => o.withSynonym(synonym)
        case s => throw new IllegalArgumentException(s"illegal synonym $s")
      }
      case Node(Leaf(ADJECTIVE) +: adjectives) => adjectives.foldLeft(obj) {
        case (o, Leaf(adjective)) => o.withAdjective(adjective)
        case s => throw new IllegalArgumentException(s"illegal adjective $s")
      }
      case Node(Seq(Leaf(DESC), Leaf(desc))) => obj.withDesc(desc)
      case Node(Leaf(FLAGS) +: flags) => flags.foldLeft(obj) {
        case (o, Leaf(flag)) => o.withFlag(Flag(flag))
        case s => throw new IllegalArgumentException(s"illegal flag $s")
      }
      case Node(Seq(Leaf(ACTION), Leaf(actionId))) => obj.withAction(Action(actionId))
      case Node(Seq(Leaf(FDESC), Leaf(fdesc))) => obj.withFDesc(fdesc)
      case Node(Seq(Leaf(LDESC), Leaf(ldesc))) => obj.withLDesc(ldesc)
      case Node(Seq(Leaf(SIZE), Leaf(size))) => obj.withSize(size.toInt)

      case _ => throw new IllegalArgumentException(s"illegal clause in object text: $clause")
    }

    val tokens = Tokenizer.tokenize(text)
    val tree = Parser.parse(tokens)

    tree match {
      case Node(Leaf(OBJECT) :: Leaf(id) :: clauses) =>
        clauses.foldLeft(Object(id)) {
          case (obj, clause) => enrichObj(obj, clause)
        }
      case _ => throw new IllegalArgumentException
    }
  }
}
