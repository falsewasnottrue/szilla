package models.syntax

trait Synonym {
  def values: Seq[String]
  def withSynonym(synonym: String): Synonym
}
case class VerbSynonym(values: Seq[String]) extends Synonym {
  def withSynonym(synonym: String): Synonym = copy(values = values :+ synonym)
}
case class PrepSynonym(values: Seq[String]) extends Synonym {
  def withSynonym(synonym: String): Synonym = copy(values = values :+ synonym)
}
case class AdjSynonym(values: Seq[String]) extends Synonym {
  def withSynonym(synonym: String): Synonym = copy(values = values :+ synonym)
}

object Synonym {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val init = zero0("VERB-SYNONYM", VerbSynonym(Nil)) orElse
    zero0("PREP-SYNONYM", PrepSynonym(Nil)) orElse
    zero0("ADJ-SYNONYM", AdjSynonym(Nil))
  val parser = ZParser[Synonym](init)(Seq(
    point0((synonym, s) => synonym.withSynonym(s))
  ))
}


