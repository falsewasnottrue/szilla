package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class RoomParserSpec extends FlatSpec with Matchers {

  trait Env {
    val text = """<ROOM LIVING-ROOM
                 |(LOC ROOMS)
                 |(DESC "This is the living Room")
                 |(EAST TO KITCHEN)
                 |(WEST TO STRANGE-PASSAGE IF CYCLOPS-FLED ELSE
                 |"The wooden door is nailed shut.")
                 |(DOWN PER TRAP-DOOR-EXIT)
                 |(ACTION LIVING-ROOM-F)
                 |(FLAGS RLANDBIT ONBIT SACREDBIT)
                 |(GLOBAL STAIRS)
                 |(THINGS <> NAILS NAILS-PSEUDO)>""".stripMargin
  }

  "RoomParser" should "parse a well-form room.zil" in new Env {
    val room = RoomParser.parse(text)

    room.id should be("LIVING-ROOM")
    room.location should be(Rooms)
    room.desc should be(Some("This is the living Room"))

    room.east should be(UExit("KITCHEN"))
    room.down should be(FExit("TRAP-DOOR-EXIT"))

    room.action should be(Some(Action("LIVING-ROOM-F")))
  }
}
