package models

import org.scalatest.{FlatSpec, Matchers}

class VariableSpec extends FlatSpec with Matchers {

  "LocalVariable" should "be a Variable" in {
    val l = LocalVariable("A")
    l.isInstanceOf[Var] should be(true)
  }

  it should "be created when a . is prefixed" in {
    var v = Variable(".LOCAL")
    v should be(LocalVariable("LOCAL"))
  }

  "GlobalVariable" should "be a Variable" in {
    val g = GlobalVariable("A")
    g.isInstanceOf[Var] should be(true)
  }

  it should "be created when a , is prefixed" in {
    var v = Variable(",GLOBAL")
    v should be(GlobalVariable("GLOBAL"))
  }

  "PropertyNameVariable" should "be a Variable" in {
    val p = PropertyNameVariable("PROP")
    p.isInstanceOf[Var] should be(true)
  }

  it should "be created when a ,P? is prefixed" in {
    var v = Variable(",P?PROP")
    v should be(PropertyNameVariable("PROP"))
  }
}
