package models.syntax

import org.scalatest.{FlatSpec, Matchers}

class SyntaxTokenSpec extends FlatSpec with Matchers {

  "SyntaxToken" should "detect tokens" in {
    SyntaxToken.unapply("HAVE") should be(Some(HAVE))
    SyntaxToken.unapply("TAKE") should be(Some(TAKE))
    SyntaxToken.unapply("MANY") should be(Some(MANY))
    SyntaxToken.unapply("EVERYWHERE") should be(Some(EVERYWHERE))
    SyntaxToken.unapply("ADJACENT") should be(Some(ADJACENT))
    SyntaxToken.unapply("HELD") should be(Some(HELD))
    SyntaxToken.unapply("CARRIED") should be(Some(CARRIED))
    SyntaxToken.unapply("ON-GROUND") should be(Some(ONGROUND))
    SyntaxToken.unapply("IN-ROOM") should be(Some(INROOM))
    SyntaxToken.unapply("FIND ROOMSBIT") should be(Some(FIND("ROOMSBIT")))

    SyntaxToken.unapply("GIBTSNICHT") should be(None)
  }
}
