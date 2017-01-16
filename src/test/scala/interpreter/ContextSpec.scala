package interpreter

import models._
import org.scalatest.{FlatSpec, Matchers}

class ContextSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
    val v = IntValue(42)
  }

  "Context" should "store variables" in new Env {
    val x = Variable("x")
    ctx.get(x) should be(None)

    ctx.set(x, v)
    ctx.get(x) should be(Some(v))
  }

  it should "manage stack" in new Env {
    ctx.pop should be(None)
    ctx.push(v)
    ctx.pop should be(Some(v))
    ctx.pop should be(None)
  }
}
