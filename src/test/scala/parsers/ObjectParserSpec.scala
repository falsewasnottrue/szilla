package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class ObjectParserSpec extends FlatSpec with Matchers {

  trait Env {
    val text="""<OBJECT LANTERN
               |(LOC LIVING-ROOM)
               |(SYNONYM LAMP LANTERN LIGHT)
               |(ADJECTIVE BRASS)
               |(DESC "brass lantern")
               |(FLAGS TAKEBIT LIGHTBIT)
               |(ACTION LANTERN-F)
               |(FDESC "A battery-powered lantern is on the trophy case.")
               |(LDESC "There is a brass lantern (battery-powered) here.")
               |(SIZE 15)>""".stripMargin
  }

  "Object Parser" should "parse a well-formed object text" in new Env {
    val obj = Object.parser.parse(text)

    obj.id should be("LANTERN")
    obj.location should be(RoomLocation("LIVING-ROOM"))
    obj.synonyms should be(Seq("LAMP", "LANTERN", "LIGHT"))
    obj.adjectives should be(Seq("BRASS"))
    obj.desc should be(Some("brass lantern"))
    obj.flags should be(Seq(Flag("TAKEBIT"), Flag("LIGHTBIT")))
    obj.action should be(Some(Action("LANTERN-F")))
    obj.fdesc should be(Some("A battery-powered lantern is on the trophy case."))
    obj.ldesc should be(Some("There is a brass lantern (battery-powered) here."))
    obj.size should be(15)
  }
}
