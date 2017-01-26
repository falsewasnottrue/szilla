package models

import org.scalatest.{FlatSpec, Matchers}

class RoomSpec extends FlatSpec with Matchers {

  "Room" should "have an ID" in {
    Room(id = "LIVING-ROOM").id should be("LIVING-ROOM")
  }

  it should "have a description" in {
    Room(id = "LIVING-ROOM").withDesc("Living Room").properties.get(PropertyName.DESC) should be(Some("Living Room"))
  }

  it should "allow to query for Exits" in {
    val room = Room(id = "LIVING-ROOM")

    room.exit(North) should be(NExit(None))
    room.exit(South) should be(NExit(None))
    room.exit(West) should be(NExit(None))
    room.exit(East) should be(NExit(None))
    room.exit(Up) should be(NExit(None))
    room.exit(Down) should be(NExit(None))
  }

  it should "have an optional (?) Action" in {
    val room = Room(id = "LIVING-ROOM")

    room.action should be(None)
  }

  it should "be in the rooms location" in {
    val room = Room(id = "LIVING-ROOM")
    room.location should be(Rooms)
  }

  it should "allow to add a description" in {
    val room = Room(id = "LIVING-ROOM")
    room.properties.get(PropertyName.DESC) should be(None)
    val r = room.withDesc("this is the description")
    r.properties.get(PropertyName.DESC) should be(Some("this is the description"))
  }

  it should "allow to add an action" in {
    val room = Room(id = "LIVING-ROOM")
    room.action should be(None)
    val r = room.withAction(Action("LIVING-ROOM-F"))
    r.action should be(Some(Action("LIVING-ROOM-F")))
  }

  it should "allow to add exits" in {
    val room = Room(id = "LIVING-ROOM")
    room.exit(East) should be(NExit(None))
    val r = room.withExit(East, UExit("KITCHEN"))
    r.exit(East) should be(UExit("KITCHEN"))
  }

  it should "allow to add flags" in {
    val r = Room(id = "LIVING-ROOM")
    r.flags should be(Nil)
    val r2 = r.withFlag(Flag("ONBIT"))
    r2.flags should be(Seq(Flag("ONBIT")))

    val r3 = r2.withFlag(Flag("SACREDBIT"))
    r2.flags should be(Seq(Flag("ONBIT")))
    r3.flags should be(Seq(Flag("ONBIT"), Flag("SACREDBIT")))
  }
}
