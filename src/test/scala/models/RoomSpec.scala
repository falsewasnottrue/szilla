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

    room.north should be(NExit)
    room.south should be(NExit)
    room.west should be(NExit)
    room.east should be(NExit)
    room.up should be(NExit)
    room.down should be(NExit)
  }

  it should "have an optional (?) Action" in {
    val room = Room(id = "LIVING-ROOM")

    room.action should be(None)
  }
}
