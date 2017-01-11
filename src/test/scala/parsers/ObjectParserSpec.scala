package parsers

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
               |(FDESC "A battery-powered lantern is on the trophy
               |case.")
               |(LDESC "There is a brass lantern (battery-powered)
               |here.")
               |(SIZE 15)>""".stripMargin
  }

  "Object Parser" should "" in new Env {

  }
}
