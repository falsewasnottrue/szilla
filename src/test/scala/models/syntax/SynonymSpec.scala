package models.syntax

import org.scalatest.{FlatSpec, Matchers}

class SynonymSpec extends FlatSpec with Matchers {

  "Synonym" should "parse verb synonyms" in {
    val text = "<VERB-SYNONYM WORRY FRET AGONIZE>"
    val verbSynonym = Synonym.parser.parse(text)

    verbSynonym.isInstanceOf[VerbSynonym] should be(true)
    verbSynonym.values should be(Seq("WORRY", "FRET", "AGONIZE"))
  }

  it should "parse prep synonyms" in {
    val text = "<PREP-SYNONYM UNDER UNDERNEATH BENEATH BELOW>"
    val prepSynonym = Synonym.parser.parse(text)

    prepSynonym.isInstanceOf[PrepSynonym] should be(true)
    prepSynonym.values should be(Seq("UNDER", "UNDERNEATH", "BENEATH", "BELOW"))
  }

  it should "parse adj synonyms" in {
    val text = "<ADJ-SYNONYM LARGE BIG GREAT HUGE>"
    val adjSynonym = Synonym.parser.parse(text)

    adjSynonym.isInstanceOf[AdjSynonym] should be(true)
    adjSynonym.values should be(Seq("LARGE", "BIG", "GREAT", "HUGE"))
  }
}
