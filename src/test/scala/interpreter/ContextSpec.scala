package interpreter

import models._
import org.scalatest.{FlatSpec, Matchers}

class ContextSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "Context" should "store variables" in new Env {
    val x = Variable("x")
    ctx.get(x) should be(None)

    val v = new Value
    val c = ctx.set(x, v)
    c.get(x) should be(Some(v))
    c.parent should be(Some(ctx))
  }
}
