package parsers

import models.Room

object RoomParser {
  def parse(text: String): Room = {
    val tokens = Tokenizer.tokenize(text)
    val tree = Parser.parse(tokens)

    tree match {
      case Node(Leaf("ROOM") :: Leaf(id) :: _) => Room(id)
      case _ => throw new IllegalArgumentException
    }
  }
}
