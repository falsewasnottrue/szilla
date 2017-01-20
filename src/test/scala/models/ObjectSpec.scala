package models

import org.scalatest.{FlatSpec, Matchers}

class ObjectSpec extends FlatSpec with Matchers {

  trait Env {
    val location = Room("LIVING-ROOM")
    val lantern = Object(id = "LANTERN", location = RefLocation(location.id))
    val desk = Object(id = "DESK")
  }

  "Object" should "have an Id" in {
    Object(id = "LANTERN").id should be("LANTERN")
  }

  it should "have a location" in new Env {
    lantern.location should be(RefLocation(location.id))
  }

  it should "allow to add a location" in {
    val o = Object(id = "LANTERN")
    o.location should be(Empty)

    val obj = o.withLocation(Player)
    obj.location should be(Player)
  }

  it should "allow to add synonyms" in new Env {
    lantern.synonyms should be(Nil)
    val o = lantern.withSynonym("LAMP")
    o.synonyms should be(Seq("LAMP"))
  }

  it should "allow to add adjectives" in new Env {
    lantern.adjectives should be(Nil)
    val o = lantern.withAdjective("BRASS")
    o.adjectives should be(Seq("BRASS"))
  }

  it should "allow to add a description" in new Env {
    lantern.properties.get(PropertyName.DESC) should be(None)
    val o = lantern.withDesc("This is a description")
    o.properties.get(PropertyName.DESC) should be(Some("This is a description"))
  }

  it should "allow to add flags" in new Env {
    lantern.flags should be(Nil)
    val o = lantern.withFlag(Flag("ONBIT"))
    o.flags should be(Seq(Flag("ONBIT")))
  }

  it should "allow to add an action" in new Env {
    lantern.action should be(None)
    val o = lantern.withAction(Action("LANTERN-F"))
    o.action should be(Some(Action("LANTERN-F")))
  }

  it should "allow to add an fDesc" in new Env {
    lantern.properties.get(PropertyName.FDESC) should be(None)
    val o = lantern.withFDesc("fdesc")
    o.properties.get(PropertyName.FDESC) should be(Some("fdesc"))
  }

  it should "allow to add an ldesc" in new Env {
    lantern.properties.get(PropertyName.LDESC) should be(None)
    val o = lantern.withLDesc("ldesc")
    o.properties.get(PropertyName.LDESC) should be(Some("ldesc"))
  }

  it should "allow to add a size" in new Env {
    lantern.properties.get(PropertyName.SIZE) should be(None)
    val o = lantern.withSize(15)
    o.properties.getInt(PropertyName.SIZE) should be(Some(15))
  }

  it should "allow to put one object into another" in new Env {
    desk.insert(lantern)

    desk.contains(lantern) should be(true)
    lantern.location should be(RefLocation(desk.id))
  }
}
