package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class SyntaxParserSpec extends FlatSpec with Matchers {

  "Syntax parser" should "parse <SYNTAX EAT OBJECT = V-EAT>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX EAT OBJECT = V-EAT>")

    syntax.verbId should be("V-EAT")
    syntax.verb should be("EAT")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

  it should "parse <SYNTAX LOOK THROUGH OBJECT = V-LOOK-INSIDE>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX LOOK THROUGH OBJECT = V-LOOK-INSIDE>")

    syntax.verbId should be("V-LOOK-INSIDE")
    syntax.verb should be("LOOK THROUGH")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

  it should "parse <SYNTAX LOOK UNDER OBJECT = V-LOOK-UNDER>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX LOOK UNDER OBJECT = V-LOOK-UNDER>")

    syntax.verbId should be("V-LOOK-UNDER")
    syntax.verb should be("LOOK UNDER")
    syntax.obj1 should be(Some(ObjectToken(Nil)))
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

  it should "parse <SYNTAX INVENTORY = V-INVENTORY>" in {
    val Some(syntax) = Syntax.unapply("<SYNTAX INVENTORY = V-INVENTORY>")

    syntax.verbId should be("V-INVENTORY")
    syntax.verb should be("INVENTORY")
    syntax.obj1 should be(None)
    syntax.prep should be(None)
    syntax.obj2 should be(None)
  }

  // <SYNTAX GIVE OBJECT TO OBJECT = V-GIVE>

  /*
  <SYNTAX GET OBJECT = V-TAKE>
  <SYNTAX GET IN OBJECT = V-ENTER>
  <SYNTAX GET ON OBJECT = V-ENTER>
  <SYNTAX GET OFF OBJECT = V-EXIT>
  <SYNTAX GET OBJECT WITH OBJECT = V-TAKE-WITH>
   */
  // <SYNTAX TAKE OBJECT (FIND TAKEBIT) = V-TAKE>
  // <SYNTAX ATTACK OBJECT WITH OBJECT (FIND WEAPONBIT)= V-ATTACK>
  // <SYNTAX FIRE AT OBJECT = V-SHOOT PRE-FIRE>

  // Tokens
  // <SYNTAX GIVE OBJECT (HAVE) TO OBJECT (ON-GROUND IN-ROOM) = V-GIVE>
  // <SYNTAX EXAMINE OBJECT (MANY) = V-EXAMINE>
}
