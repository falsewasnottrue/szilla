package interpreter

import models.{GlobalVariable, IntValue, Routine}
import org.mockito.{ArgumentCaptor, Mockito}
import org.scalatest._

class RunSpec extends FlatSpec with Matchers {

  it should "run a routine" in {
    val text = """<ROUTINE INCREMENT-SCORE (NUM)
                 | <SETG SCORE <+ ,SCORE .NUM>>>
               """.stripMargin
    val routine = Routine.parser.parse(text)
    Global.registerRoutine(routine)
    Global.define(GlobalVariable("SCORE"), IntValue(19))

    val go = Routine.parser.parse("<ROUTINE GO () <CALL INCREMENT-SCORE 23>>")
    Global.registerRoutine(go)
    val startCtx = Context(Ip(go, 0))

    val ctx = Interpreter.run(startCtx)

    ctx.getGlobal(GlobalVariable("SCORE")) should be(IntValue(42))
  }

  it should "run a slightly longer routine" in {
    val text1 = """<ROUTINE LINE-IN-RHYME (ARG-A ARG-B)
                     |  <TELL .ARG-A " days hath " .ARG-B CR>>""".stripMargin
    val routine1 = Routine.parser.parse(text1)
    Global.registerRoutine(routine1)

    val text2 = """<ROUTINE RHYME ("AUX" ARG1 ARG2)
                  |    <SET ARG1 30>
                  |    <SET ARG2 "September">
                  |    <CALL LINE-IN-RHYME .ARG1 .ARG2>
                  |    <SET ARG1 28>
                  |    <SET ARG2 "February">
                  |    <CALL LINE-IN-RHYME .ARG1 .ARG2>>""".stripMargin
    val routine2 = Routine.parser.parse(text2)
    Global.registerRoutine(routine2)

    val go = Routine.parser.parse("<ROUTINE GO () <CALL RHYME>>")
    Global.registerRoutine(go)
    val startCtx = Context(Ip(go, 0))
    val mockedStartCtx = Mockito.spy(startCtx)

    val ctx = Interpreter.run(mockedStartCtx)

    val captor = ArgumentCaptor.forClass(classOf[String])
    Mockito.verify(mockedStartCtx, Mockito.times(8)).out(captor.capture())

    captor.getAllValues should contain("September")
    captor.getAllValues should contain("February")
  }
}
