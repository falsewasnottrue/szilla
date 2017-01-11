package models

import org.scalatest.{FlatSpec, Matchers}

class ObjectSpec extends FlatSpec with Matchers {

  trait Env {
    val location = RoomLocation("LIVING-ROOM")
    val obj = Object(id = "LANTERN", location = location)
  }

  "Object" should "have an Id" in {
    Object(id = "LANTERN").id should be("LANTERN")
  }

  it should "have a location" in new Env {
    obj.location should be(location)
  }

  it should "allow to add a location" in {
    val o = Object(id = "LANTERN")
    o.location should be(Empty)

    val obj = o.withLocation(Player)
    obj.location should be(Player)
  }

  it should "allow to add synonyms" in new Env {
    obj.synonyms should be(Nil)
    val o = obj.withSynonym("LAMP")
    o.synonyms should be(Seq("LAMP"))
  }

  it should "allow to add adjectives" in new Env {
    obj.adjectives should be(Nil)
    val o = obj.withAdjective("BRASS")
    o.adjectives should be(Seq("BRASS"))
  }

  it should "allow to add a description" in new Env {
    obj.desc should be(None)
    val o = obj.withDesc("This is a description")
    o.desc should be(Some("This is a description"))
  }

  it should "allow to add flags" in new Env {
    obj.flags should be(Nil)
    val o = obj.withFlag(Flag("ONBIT"))
    o.flags should be(Seq(Flag("ONBIT")))
  }

  it should "allow to add an action" in new Env {
    obj.action should be(None)
    val o = obj.withAction(Action("LANTERN-F"))
    o.action should be(Some(Action("LANTERN-F")))
  }

  it should "allow to add an fDesc" in new Env {
    obj.fdesc should be(None)
    val o = obj.withFDesc("fdesc")
    o.fdesc should be(Some("fdesc"))
  }

  it should "allow to add an ldesc" in new Env {
    obj.ldesc should be(None)
    val o = obj.withLDesc("ldesc")
    o.ldesc should be(Some("ldesc"))
  }

  it should "allow to add a size" in new Env {
    obj.size should be(0)
    val o = obj.withSize(15)
    o.size should be(15)
  }
}
