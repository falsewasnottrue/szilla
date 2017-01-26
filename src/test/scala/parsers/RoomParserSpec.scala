package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class RoomParserSpec extends FlatSpec with Matchers {

  trait Env {
    val text = """<ROOM LIVING-ROOM
                 |(LOC ROOMS)
                 |(DESC "This is the living Room")
                 |(EAST TO KITCHEN)
                 |(WEST TO STRANGE-PASSAGE IF CYCLOPS-FLED ELSE "The wooden door is nailed shut.")
                 |(NW SORRY "The soldier informs you that only Emperor Bonaparte is allowed through.")
                 |(DOWN PER TRAP-DOOR-EXIT)
                 |(ACTION LIVING-ROOM-F)
                 |(FLAGS RLANDBIT ONBIT SACREDBIT)
                 |(GLOBAL STAIRS)
                 |(THINGS <> NAILS NAILS-PSEUDO)>""".stripMargin
  }

  "RoomParser" should "parse a well-form room text" in new Env {
    val room = Room.parser.parse(text)

    room.id should be("LIVING-ROOM")
    room.location should be(Rooms)
    room.properties.get(PropertyName.DESC) should be(Some("This is the living Room"))

    room.exit(East) should be(UExit("KITCHEN"))
    room.exit(West) should be(CExit("STRANGE-PASSAGE", Variable("CYCLOPS-FLED"), "The wooden door is nailed shut."))
    room.exit(Down) should be(FExit("TRAP-DOOR-EXIT"))
    room.exit(NW) should be(NExit(Some("The soldier informs you that only Emperor Bonaparte is allowed through.")))
    room.action should be(Some(Action("LIVING-ROOM-F")))
    room.flags should be(Seq(Flag("RLANDBIT"), Flag("ONBIT"), Flag("SACREDBIT")))
  }
}
