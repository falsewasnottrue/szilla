package interpreter.impl

import org.mockito.Mockito

class CRLFInterpreterSpec extends BaseInterpreterSpec {


  "CRLFInterpreter" should "print a carriage return" in new Env {
    val spyCtx = Mockito.spy(ctx)
    run(spyCtx)("<CRLF>")

    Mockito.verify(spyCtx).out("\n")
  }

  it should "fail with parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<CRLF 1>")
    }
  }
}