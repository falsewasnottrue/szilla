package interpreter.impl

import models.{IntValue, TableValue}

class TableInterpreterSpec extends BaseInterpreterSpec {

  "TableInterpreter" should "generate a table on the stack" in new Env {
    run(ctx)("<TABLE 1 2 3>").pop should be(Some(TableValue(
      Map(0 -> IntValue(1), 1 -> IntValue(2), 2 -> IntValue(3))
    )))
  }

}
