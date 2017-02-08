package models.syntax

import org.scalatest.{FlatSpec, Matchers}

class SyntaxParserSpec extends FlatSpec with Matchers {

  "Syntax parser" should "parse <SYNTAX EAT OBJECT = V-EAT>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX EAT OBJECT = V-EAT>")

    syntax.verbId should be("V-EAT")
    syntax.preAction should be(None)

    syntax.verb should be("EAT")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }


  it should "parse <SYNTAX LOOK THROUGH OBJECT = V-LOOK-INSIDE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX LOOK THROUGH OBJECT = V-LOOK-INSIDE>")

    syntax.verbId should be("V-LOOK-INSIDE")
    syntax.preAction should be(None)

    syntax.verb should be("LOOK THROUGH")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

 it should "parse <SYNTAX LOOK UNDER OBJECT = V-LOOK-UNDER>" in {
   val Some(syntax) = Syntax.unapply("<SYNTAX LOOK UNDER OBJECT = V-LOOK-UNDER>")

   syntax.verbId should be("V-LOOK-UNDER")
   syntax.preAction should be(None)

   syntax.verb should be("LOOK UNDER")
   syntax.obj1 should be(Some(ObjectToken(Nil)))
   syntax.prep should be(None)
   syntax.obj2 should be(None)
 }

 it should "parse <SYNTAX INVENTORY = V-INVENTORY>" in {
   val Some(syntax) = Syntax.unapply("<SYNTAX INVENTORY = V-INVENTORY>")

   syntax.verbId should be("V-INVENTORY")
   syntax.preAction should be(None)

   syntax.verb should be("INVENTORY")
   syntax.obj1 should be(None)
   syntax.prep should be(None)
   syntax.obj2 should be(None)
 }

  it should "parse <SYNTAX GIVE OBJECT TO OBJECT = V-GIVE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX GIVE OBJECT TO OBJECT = V-GIVE>")

    syntax.verbId should be("V-GIVE")
    syntax.preAction should be(None)

    syntax.verb should be("GIVE")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(Some("TO"))
    syntax.obj2 should be(Some(ObjectToken(Nil)))
  }

  it should "parse <SYNTAX TAKE OBJECT (FIND TAKEBIT) = V-TAKE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX TAKE OBJECT (FIND TAKEBIT) = V-TAKE>")

    syntax.verbId should be("V-TAKE")
    syntax.preAction should be(None)

    syntax.verb should be("TAKE")
    syntax.obj1 should be(Some(ObjectToken(Seq(FIND("TAKEBIT")))))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

  it should "parse <SYNTAX ATTACK OBJECT WITH OBJECT (FIND WEAPONBIT) = V-ATTACK>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX ATTACK OBJECT WITH OBJECT (FIND WEAPONBIT) = V-ATTACK>")

    syntax.verbId should be("V-ATTACK")
    syntax.preAction should be(None)
    syntax.verb should be("ATTACK")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(Some("WITH"))
    syntax.obj2 should be(Some(ObjectToken(Seq(FIND("WEAPONBIT")))))
  }

  it should "parse <SYNTAX FIRE AT OBJECT = V-SHOOT PRE-FIRE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX FIRE AT OBJECT = V-SHOOT PRE-FIRE>")

    syntax.verbId should be("V-SHOOT")
    syntax.preAction should be(Some("PRE-FIRE"))

    syntax.verb should be("FIRE AT")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

  it should "parse <SYNTAX GIVE OBJECT (HAVE) TO OBJECT (ON-GROUND IN-ROOM) = V-GIVE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX GIVE OBJECT (HAVE) TO OBJECT (ON-GROUND IN-ROOM) = V-GIVE>")

    syntax.verbId should be("V-GIVE")
    syntax.preAction should be(None)

    syntax.verb should be("GIVE")
    syntax.obj1 should be(Some(ObjectToken(Seq(HAVE))))
    syntax.prep should be(Some("TO"))
    syntax.obj2 should be(Some(ObjectToken(Seq(ONGROUND, INROOM))))
  }

  it should "parse <SYNTAX EXAMINE OBJECT (MANY) = V-EXAMINE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX EXAMINE OBJECT (MANY) = V-EXAMINE>")

    syntax.verbId should be("V-EXAMINE")
    syntax.preAction should be(None)

    syntax.verb should be("EXAMINE")
    syntax.obj1 should be(Some(ObjectToken(Seq(MANY))))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }
}
