package models

case class Routine(
                    id: Id)
  extends HasId {

}

object Routine {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Routine](zero(ROUTINE, Routine(_)))(Seq(
    // TODO
  ))
}