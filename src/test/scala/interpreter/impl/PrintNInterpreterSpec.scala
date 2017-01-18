package interpreter.impl

import org.mockito.Mockito

class PrintNInterpreterSpec extends BaseInterpreterSpec {

  "PrintInterpreter" should "print the given number to the current window" in new Env {
    val ctxSpy = Mockito.spy(ctx)
    run(ctxSpy)("<PRINTN 123>")

    Mockito.verify(ctxSpy).out("123")
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINTN 123 456>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINTN>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINTN \"Not bloody likely!\">")
    }
  }
}
