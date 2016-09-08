package models

import org.scalatest.{FlatSpec, Matchers}

class RoomSpec extends FlatSpec with Matchers {

  "Room" should "have an ID" in {
    Room(id = "LIVING-ROOM").id should be("LIVING-ROOM")
  }

  it should "have a description" in {
    Room(id = "LIVING-ROOM", desc = Some("Living Room")).desc should be(Some("Living Room"))
  }
}
