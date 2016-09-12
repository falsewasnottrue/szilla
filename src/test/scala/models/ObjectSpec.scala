package models

import org.scalatest.{FlatSpec, Matchers}

class ObjectSpec extends FlatSpec with Matchers {

  "Object" should "have an Id" in {
    Object(id = "LANTERN", location = new Location()).id should be("LANTERN")
  }

  it should "have a location" in {
    val location = new Location()
    val obj = Object(id = "LANTERN", location = location)
    obj.location should be(location)
  }
}
