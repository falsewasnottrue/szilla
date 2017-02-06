package interpreter.impl

import interpreter.{Global, InstructionPointer}
import models.{Routine, StringValue}
import org.mockito.Mockito

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class InputInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val text =
      """<ROUTINE DUMMY ()>
      """.stripMargin
    val routine = Routine.parser.parse(text)
    Global.registerRoutine(routine)
  }

  "InputInterpreter" should "read a single key from in" in new Env0 {
    val ctxMock = Mockito.spy(ctx)
    Mockito.when(ctxMock.in).thenReturn(Future.successful("Y"))

    // wait for 5/10 seconds, then call dummy
    run(ctxMock)("<INPUT 1>").pop should be(Some(StringValue("Y")))
  }

  it should "call the routine if no key was pressed in time" in new Env0 {
    val ctxMock = Mockito.spy(ctx)
    Mockito.when(ctxMock.in).thenReturn(Future { Thread.sleep(2000); "Y"})

    val c = run(ctxMock)("<INPUT 1 5 DUMMY>")
    c.parent should be(Some(ctxMock))
    c.ip should be(InstructionPointer(routine, 0))
  }

  // takes too long (waiting for timeout)
  // it should "return an empty string if no key was pressed in time" in new Env0 {
  //  val ctxMock = Mockito.spy(ctx)
  //  Mockito.when(ctxMock.in).thenReturn(Future { Thread.sleep(20000); "Y"})
  //  run(ctxMock)("<INPUT 1>").pop should be(Some(StringValue("")))
  //}

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<INPUT 1 5 DUMMY <RTRUE>>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("INPUT 1 5")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("INPUT 5 DUMMY 1")
    }
  }
}
