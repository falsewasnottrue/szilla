package parsers

import models._
import KeyWords._

object RoomParser {
  def parse(text: String): Room = {
    def enrichRoom(room: Room, clause: Tree): Room = clause match {
      // desc
      case Node(Seq(Leaf(DESC), Leaf(desc))) => room.withDesc(desc)
      // exits
      case Node(Seq(Leaf(Direction(dir)), Leaf(TO), Leaf(roomId))) => room.withExit(dir, UExit(roomId))
      case Node(Seq(Leaf(Direction(dir)), Leaf(PER), Leaf(exitId))) => room.withExit(dir, FExit(exitId))
      // TODO conditional exit
      // action
      case Node(Seq(Leaf(ACTION), Leaf(actionId))) => room.withAction(Action(actionId))
      // flags
      case Node(Leaf(FLAGS) +: flags) => flags.foldLeft(room) {
        case (r, Leaf(flagId)) => r.withFlag(Flag(flagId))
        case (_, f) => throw new IllegalArgumentException(s"illegal flag: $f")
      }
      // TODO global
      // TODO things
      case _ => room // TODO exception when all other cases are implemented
    }

    val tokens = Tokenizer.tokenize(text)
    val tree = Parser.parse(tokens)

    tree match {
      case Node(Leaf(ROOM) :: Leaf(id) :: clauses) =>
        clauses.foldLeft(Room(id)) {
          case (room, clause) => enrichRoom(room, clause)
        }
      case _ => throw new IllegalArgumentException
    }
  }
}
