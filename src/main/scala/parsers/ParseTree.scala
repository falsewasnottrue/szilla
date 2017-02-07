package parsers

sealed trait ParseTree
case class Leaf(value: String) extends ParseTree
case class Node(nodes: Seq[ParseTree], parType: ParType = Angle) extends ParseTree

