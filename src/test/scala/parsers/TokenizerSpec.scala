package parsers

import org.scalatest.{FlatSpec, Matchers}

class TokenizerSpec extends FlatSpec with Matchers {

  "Tokenizer" should "tokenize room.zil" in {
    val text = "<ROOM LIVING-ROOM (THINGS <> NAILS NAILS-PSEUDO)>"
    val tokens = Tokenizer.tokenize(text)

    tokens should be(Seq("<", "ROOM", "LIVING-ROOM", "<", "THINGS", "<", ">", "NAILS", "NAILS-PSEUDO", ">", ">"))
  }

  it should "handle quotes" in {
    val text = """(DESC "This is the description")"""
    val tokens = Tokenizer.tokenize(text)

    tokens should be(Seq("<", "DESC", "This is the description", ">"))
  }
}
