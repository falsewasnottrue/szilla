package parsers

import models._

object RoomParser {
  def parse(text: String): Room = {
    val tokens = Tokenizer.tokenize(text)
    val tree = Parser.parse(tokens)


    def enrichRoom(room: Room, tree: Tree): Room = {
      def isDirection(s: String): String = ""
      tree match {
        // TODO desc
        case Node(Seq(Leaf("DESC"), Leaf(desc))) => room.withDesc(desc)
        // exits
        case Node(Seq(Leaf(Direction(dir)), Leaf("TO"), Leaf(roomId))) => room.withExit(dir, ExitTo(roomId))
        case Node(Seq(Leaf(Direction(dir)), Leaf("PER"), Leaf(exitId))) => room.withExit(dir, ExitPer(exitId))
        // TODO exit if

        // TODO action
        // TODO global
        // TODO flags
        // TODO things
        case _ => room // TODO exception?
      }
    }

    tree match {
      case Node(Leaf("ROOM") :: Leaf(id) :: rest) =>
        rest.foldLeft(Room(id)) {
          case (room, tree) => enrichRoom(room, tree)
        }
      case _ => throw new IllegalArgumentException
    }
  }
}
