package models

case class Syntax() {

}

object Syntax {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Syntax](zero0(SYNTAX, Syntax()))(Seq(
    // TODO
  ))
}
