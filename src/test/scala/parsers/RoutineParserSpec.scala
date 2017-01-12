package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class RoutineParserSpec extends FlatSpec with Matchers {

  trait Env {
    val text1 =
      """<ROUTINE TURN-OFF-HOUSE-LIGHTS ()
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

    routine1.arguments should be(Nil)

    routine1.instructions should be(Seq(
      Instruction(FCLEAR, Seq(Variable(",LIVING-ROOM"), Variable(",ONBIT"))),
      Instruction(FCLEAR, Seq(Variable(",DINING-ROOM"), Variable(",ONBIT"))),
      Instruction(FCLEAR, Seq(Variable(",KITCHEN"), Variable(",ONBIT")))
    ))
  }

//  it should "parse cond instructions" in new Env {
//    val routine2 = Routine.parser.parse(text2)
//    routine2.id should be("INCREMENT-SCORE")
//    routine2.arguments should be (Seq("NUM"))
//
//    routine2.instructions should be(Seq(
//      Instruction(SETG, )
////      <SETG SCORE <+ ,SCORE .NUM>>
////      <COND (,SCORE-NOTIFICATION-ON
////      <TELL "[Your score has just gone up by "
////    N .NUM ".]" CR>)>>
//    ))
//  }
}
