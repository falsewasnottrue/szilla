package models

import org.scalatest.{FlatSpec, Matchers}

class ObjectTokenSpec extends FlatSpec with Matchers {

  "ObjectToken" should "parse simple object token" in {
    ObjectToken.unapply("OBJECT") should be(Some(ObjectToken(Nil)))
  }

  it should "parse object tokens with single syntax token" in {
    ObjectToken.unapply("OBJECT (HAVE)") should be(Some(ObjectToken(Seq(HAVE))))
  }

  it should "parse object tokens with multiple syntax token" in {
    ObjectToken.unapply("OBJECT (HAVE ADJACENT IN-ROOM)") should be(Some(ObjectToken(Seq(HAVE, ADJACENT, INROOM))))
  }

  it should "parse object tokens with find token" in {
    ObjectToken.unapply("OBJECT (FIND TAKEBIT)") should be(Some(ObjectToken(Seq(FIND("TAKEBIT")))))
  }
}
