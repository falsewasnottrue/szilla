package parsers

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  "Parser" should "parse empty collection" in {
    Parser.parse(Seq("<", ">")) should be(Node(Seq()))
  }

  it should "parse non-empty collection" in {
    Parser.parse(Seq("<", "A", ">")) should be(Node(Seq(Leaf("A"))))
  }

  it should "build a map" in {
    val tokens = Seq("<", "ROOM", "LIVING-ROOM", "<", "THINGS", "<", ">", "NAILS", "NAILS-PSEUDO", ">", ">")
    val result = Parser.parse(tokens)

    result should be(
      Node(Seq(
        Leaf("ROOM"),
        Leaf("LIVING-ROOM"),
        Node(Seq(
          Leaf("THINGS"),
          Node(Seq()),
          Leaf("NAILS"),
          Leaf("NAILS-PSEUDO")
        ))
      ))
    )
  }
}
