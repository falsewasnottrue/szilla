package parsers

import models.{FCLEAR, Instruction, Routine}
import org.scalatest.{FlatSpec, Matchers}

class RoutineParserSpec extends FlatSpec with Matchers {

  trait Env {
    val text1 =
      """<ROUTINE TURN-OFF-HOUSE-LIGHTS (A)
        |          <FCLEAR ,LIVING-ROOM ,ONBIT>
        |          <FCLEAR ,DINING-ROOM ,ONBIT>
        |          <FCLEAR ,KITCHEN ,ONBIT>>
      """.stripMargin

     val text2 =
       """<ROUTINE INCREMENT-SCORE (NUM)
        |          <SETG SCORE <+ ,SCORE .NUM>>
        |          <COND (,SCORE-NOTIFICATION-ON
        |               <TELL "[Your score has just gone up by "
        |                    N .NUM ".]" CR>)>>
      """.stripMargin
  }

  "Routine parser" should "parse well-formed routine texts" in new Env {
    val routine1 = Routine.parser.parse(text1)
    routine1.id should be("TURN-OFF-HOUSE-LIGHTS")

    routine1.arguments should be(Seq("A"))

    routine1.instructions should be(Seq(
      Instruction(FCLEAR, Seq(",LIVING-ROOM", ",ONBIT")),
      Instruction(FCLEAR, Seq(",DINING-ROOM", ",ONBIT")),
      Instruction(FCLEAR, Seq(",KITCHEN", ",ONBIT"))
    ))
  }
}
