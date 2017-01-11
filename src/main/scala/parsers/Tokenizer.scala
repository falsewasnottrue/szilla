package parsers

import scala.collection.:+

object Tokenizer {

  def tokenize(text: String): Seq[String] = text.foldLeft((Seq.empty[String], false)) {
    // open quote
    case ((result, false), '\"') =>
      (result :+ "", true)

    // close quote
    case ((result, true), '\"') =>
      (result :+ "", false)

    // inside quote
    case ((result :+ last, true), c) =>
      (result :+ (last + c), true)

    case ((result, inQuote), c) if c.isWhitespace =>
      (result :+ "", inQuote)
    case ((result, inQuote), c) if Seq('<', '>', '(', ')') contains c =>
      (result :+ normalize(c), inQuote)

    case ((result, inQuote), c) if result == Seq.empty[String] =>
      (Seq(c.toString), inQuote)

    case ((result :+ last, inQuote), c) if Seq("<", ">", "(", ")") contains last =>
      ((result :+ last) :+ normalize(c), inQuote)
    case ((result :+ last, inQuote), c)  =>
      (result :+ (last + c), inQuote)
  }._1.filter(_ != "")

  private def normalize(c: Char): String = c match {
    case '(' => "<"
    case ')' => ">"
    case c => c.toString
  }
}
