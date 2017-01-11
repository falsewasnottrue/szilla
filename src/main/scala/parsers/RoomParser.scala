package parsers

import models._

//object RoomKeyWords {
//  val TO = "TO"
//}

object RoomParser {
  def parse(text: String): Room = {
    val tokens = Tokenizer.tokenize(text)
    val tree = Parser.parse(tokens)

    def enrichRoom(room: Room, tree: Tree): Room = tree match {
        // TODO desc
      case Node(Seq(Leaf("DESC"), Leaf(desc))) => room.withDesc(desc)
        // exits
      case Node(Seq(Leaf("NORTH"), Leaf("TO"), Leaf(roomId))) => room.withExit(North, ExitTo(roomId))
      case Node(Seq(Leaf("SOUTH"), Leaf("TO"), Leaf(roomId))) => room.withExit(South, ExitTo(roomId))
      case Node(Seq(Leaf("WEST"), Leaf("TO"), Leaf(roomId))) => room.withExit(West, ExitTo(roomId))
      case Node(Seq(Leaf("EAST"), Leaf("TO"), Leaf(roomId))) => room.withExit(East, ExitTo(roomId))
      case Node(Seq(Leaf("UP"), Leaf("TO"), Leaf(roomId))) => room.withExit(Up, ExitTo(roomId))
      case Node(Seq(Leaf("DOWN"), Leaf("TO"), Leaf(roomId))) => room.withExit(Down, ExitTo(roomId))
        // TODO exit per
        // TODO exit if

        // TODO action
        // TODO global
        // TODO flags
        // TODO things
      case _ => room // TODO exception?
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
