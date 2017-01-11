package models

import org.scalatest.{FlatSpec, Matchers}

class RoomSpec extends FlatSpec with Matchers {

  "Room" should "have an ID" in {
    Room(id = "LIVING-ROOM").id should be("LIVING-ROOM")
  }

  it should "have a description" in {
    Room(id = "LIVING-ROOM", desc = Some("Living Room")).desc should be(Some("Living Room"))
  }

  it should "allow to query for Exits" in {
    val room = Room(id = "LIVING-ROOM")

    room.north should be(NoExit)
    room.south should be(NoExit)
    room.west should be(NoExit)
    room.east should be(NoExit)
    room.up should be(NoExit)
    room.down should be(NoExit)
  }

  it should "have an optional (?) Action" in {
    val room = Room(id = "LIVING-ROOM")

    room.action should be(None)
  }

  it should "be in the rooms location" in {
    val room = Room(id = "LIVING-ROOM")
    room.location should be(Rooms)
  }

  it should "allow to add exits" in {
    val room = Room(id = "LIVING-ROOM")
    room.east should be(NoExit)
    val r = room.withExit(East, ExitTo("KITCHEN"))
    r.east should be(ExitTo("KITCHEN"))
  }
}
