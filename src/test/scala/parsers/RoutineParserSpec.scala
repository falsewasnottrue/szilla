package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class RoutineParserSpec extends FlatSpec with Matchers {

  "Routine parser" should "parse well-formed routine texts" in {
    val text1 = """<ROUTINE TURN-OFF-HOUSE-LIGHTS ()
      |          <FCLEAR ,LIVING-ROOM ,ONBIT>
      |          <FCLEAR ,DINING-ROOM ,ONBIT>
      |          <FCLEAR ,KITCHEN ,ONBIT>>
    """.stripMargin
    val routine1 = Routine.parser.parse(text1)

    routine1.id should be("TURN-OFF-HOUSE-LIGHTS")
    routine1.arguments should be(Nil)

    routine1.instructions should be(Seq(
      Instruction(FCLEAR, Seq(Variable(",LIVING-ROOM"), Variable(",ONBIT"))),
      Instruction(FCLEAR, Seq(Variable(",DINING-ROOM"), Variable(",ONBIT"))),
      Instruction(FCLEAR, Seq(Variable(",KITCHEN"), Variable(",ONBIT")))
    ))
  }

  it should "parse cond instructions" in {
    val text2 =
      """<ROUTINE INCREMENT-SCORE (NUM)
        |          <SETG SCORE <+ ,SCORE .NUM>>
        |          <COND (,SCORE-NOTIFICATION-ON
        |               <TELL "[Your score has just gone up by "
        |                    N .NUM ".]" CR>)>>
      """.stripMargin
    val routine2 = Routine.parser.parse(text2)

    routine2.id should be("INCREMENT-SCORE")
    routine2.arguments should be (Seq(SimpleArgument("NUM")))

    routine2.instructions should be(Seq(
      Instruction(SETG, Seq(
        Variable("SCORE"),
        Instruction(ADD, Seq(Variable(",SCORE"), Variable(".NUM")))
      )),
      Instruction(COND, Seq(
        Condition(
          Variable(",SCORE-NOTIFICATION-ON"),
          Instruction(TELL, Seq(
            Variable("[Your score has just gone up by "),
            Variable("N"),
            Variable(".NUM"),
            Variable(".]"),
            Variable("CR")
          ))
        )
      ))
    ))
  }

  it should "parse AUX and OPT parameters" in {
    val text =
      """<ROUTINE R (A B "AUX" C D "OPT" E F)>
      """.stripMargin
    val routine = Routine.parser.parse(text)

    routine.id should be("R")
    routine.arguments should be(Seq(
      SimpleArgument("A"),
      SimpleArgument("B"),
      AuxArgument("C"),
      SimpleArgument("D"),
      OptArgument("E"),
      SimpleArgument("F")
    ))
  }

    // TODOs
    // T guard
    // Return-VALUE

//    """
//      |<ROUTINE FIND-FOOD ("AUX" FOOD)
//      |          <COND (<IN? ,HAM-SANDWICH ,HERE>
//      |                 <SET FOOD ,HAM-SANDWICH>)
//      |                (<IN? ,CANDY-BAR ,HERE>
//      |                 <SET FOOD ,CANDY-BAR>)
//      |                (<IN? ,BELGIAN-ENDIVE ,HERE>
//      |                 <SET FOOD ,BELGIAN-ENDIVE>)
//      |                (T
//      |                 <SET FOOD <>)>
//      |.FOOD>
//    """.stripMargin

}
