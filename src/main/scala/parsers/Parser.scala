package parsers

sealed trait Tree
case class Leaf(value: String) extends Tree
case class Node(nodes: Seq[Tree]) extends Tree

object Parser {
  def parse(tokens: Seq[String]): Tree = Node(tokens.foldLeft(Seq.empty[Tree]) {
    case (acc, token) if Seq("(", "<") contains token => acc // TODO
    case (acc, token) if Seq(")", ">") contains token => acc // TODO
    case (acc, token) => acc :+ Leaf(token)
  })
}
