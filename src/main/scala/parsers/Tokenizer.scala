package parsers

import scala.collection.:+

object Tokenizer {

  def tokenize(text: String): Seq[String] = text.foldLeft(Seq.empty[String]) {
    case (result, c) if c.isWhitespace => result :+ ""
    case (result, c) if Seq('<', '>', '(', ')') contains c => result :+ normalize(c)

    case (result, c) if result == Seq.empty[String] => Seq(c.toString)

    case (result :+ last , c) if Seq("<", ">", "(", ")") contains last => (result :+ last) :+ normalize(c)
    case (result :+ last , c)  => result :+ (last + c)
  }.filter(_ != "")

  private def normalize(c: Char): String = c match {
    case '(' => "<"
    case ')' => ">"
    case c => c.toString
  }
}
