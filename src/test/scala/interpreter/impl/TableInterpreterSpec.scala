package interpreter.impl

import models.{IntValue, TableValue}

import scala.collection.mutable

class TableInterpreterSpec extends BaseInterpreterSpec {

  "TableInterpreter" should "generate a table on the stack" in new Env {
    run(ctx)("<TABLE 1 2 3>").pop should be(Some(TableValue(
      mutable.Map(0 -> IntValue(1), 1 -> IntValue(2), 2 -> IntValue(3))
    )))
  }

}
