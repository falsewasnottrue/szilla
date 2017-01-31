package interpreter.impl

import interpreter.Global
import models.{GlobalVariable, IntValue}

class RepeatInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    Global.define(GlobalVariable("A"), IntValue(0))
  }

  "RepeatInterpreter" should "run instructions in block" in new Env0 {
    val text =
      """
        |<REPEAT ()
        | <SETG A 1>
        | <SETG A 2>
        | <RETURN>
        |>
      """.stripMargin
    run(ctx)(text)

    ctx.getGlobal(GlobalVariable("A")) should be(IntValue(2))
  }
}
