package interpreter.impl

import interpreter.Global
import models._

class CallInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val text =
      """<ROUTINE R (A B "AUX" C D "OPT" E F)
        |<TELL "Hallo Welt!">
        |>
      """.stripMargin
    val routine = Routine.parser.parse(text)
    Global.registerRoutine(routine)
  }

  "CallInterpreter" should "open a new context with the called routine" in new Env0 {
    val c = run(ctx)("<CALL R 42 17>")

    c.parent should be(Some(ctx))
    // parameters are bound to arguments
    c.get(LocalVariable("A")) should be(IntValue(42))
    c.get(LocalVariable("B")) should be(IntValue(17))

    // parameters are not visible in the outer scope
    intercept[IllegalArgumentException] {
      ctx.get(LocalVariable("A"))
    }

    // AUX and OPT parameters are initialised with default value
    c.get(LocalVariable("C")) should be(BoolValue(false))
    c.get(LocalVariable("D")) should be(BoolValue(false))
    c.get(LocalVariable("E")) should be(BoolValue(false))
    c.get(LocalVariable("F")) should be(BoolValue(false))
  }

  it should "fill up optional parameters" in new Env0 {
    val c = run(ctx)("<CALL R 42 17 <RTRUE> \"abc\">")
    c.get(LocalVariable("E")) should be(BoolValue(true))
    c.get(LocalVariable("F")) should be(StringValue("abc"))
  }

  it should "fail without a routine name" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<CALL>")
    }
  }

  it should "fail with too few parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<CALL R 42>")
    }
  }

  it should "fail with too many parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<CALL R 42 17 1 2 3>")
    }
  }
}
