package interpreter.impl

import org.mockito.Mockito

class PrintInterpreterSpec extends BaseInterpreterSpec {

  "PrintInterpreter" should "print the given string to the current window" in new Env {
    val ctxSpy = Mockito.spy(ctx)
    run(ctxSpy)("<PRINT \"Not bloody likely!\">")

    Mockito.verify(ctxSpy).out("Not bloody likely!")
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINT \"Not bloody likely!\" 123>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINT>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINT 123>")
    }
  }
}
