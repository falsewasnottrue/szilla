package parsers

import scala.collection.:+

object Tokenizer {

  def tokenize(text: String): Seq[String] = text.foldLeft(Seq.empty[String]) {
    case (result, c) if c.isWhitespace => result :+ ""
    case (result, c) if Seq('<', '>', '(', ')') contains c => result :+ c.toString

    case (result, c) if result == Seq.empty[String] => Seq(c.toString)

    case (result :+ last , c) if Seq("<", ">", "(", ")") contains last => (result :+ last) :+ c.toString
    case (result :+ last , c)  => result :+ (last + c)
  }.filter(_ != "")
}
