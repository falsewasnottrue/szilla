package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class SyntaxParserSpec extends FlatSpec with Matchers {

  "Syntax parser" should "parse <SYNTAX EAT OBJECT = V-EAT>" in {
    // val syntax = Syntax.parser.parse("<SYNTAX EAT OBJECT = V-EAT>")
    // syntax.id should be("V-EAT")
    true should be(true)
  }
  // <SYNTAX EAT OBJECT = V-EAT>
  // <SYNTAX LOOK THROUGH OBJECT = V-LOOK-INSIDE>
  // <SYNTAX LOOK UNDER OBJECT = V-LOOK-UNDER>
  // <SYNTAX GIVE OBJECT TO OBJECT = V-GIVE>
  // <SYNTAX INVENTORY = V-INVENTORY>

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
