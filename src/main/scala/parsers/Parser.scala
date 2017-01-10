package parsers

sealed trait Tree
case class Leaf(value: String) extends Tree
case class Node(nodes: Seq[Tree]) extends Tree

object Parser {
  def parse(tokens: Seq[String]): Tree = {
    val stack: Seq[Tree] = tokens.foldLeft(Seq[Tree]()) {
      case (acc, token) => token match {
        case ">" => {
          // acc partitionieren l + "<" + r
          val (l, r) = (acc.takeWhile(_ != Leaf("<")), acc.dropWhile(_ != Leaf("<")).drop(1))
          Node(l.reverse) +: r
        }
        case lit => Leaf(lit) +: acc
      }
    }

    stack match {
      case node :: Nil => node
      case _ => throw new IllegalArgumentException
    }
  }


//  def parse(tokens: Seq[String]): Tree = tokens match {
//    case Nil => Node(Seq())
//    case "<" +: rest :+ ">" => Node(Seq())
//    case "<" +: rest => throw new IllegalArgumentException
//    case rest :+ ">" => throw new IllegalArgumentException
//    case "(" +: rest :+ ")" => Node(Seq())
//    case "(" +: rest => throw new IllegalArgumentException
//    case rest :+ ")" => throw new IllegalArgumentException
//    case a +: rest => Leaf(a) +: parse(rest)
//  }

//  tokens should be(Seq("<", "ROOM", "LIVING-ROOM", "(", "THINGS", "<", ">", "NAILS", "NAILS-PSEUDO", ")", ">"))
//  tokens should be(Seq("ROOM", "LIVING-ROOM", "(", "THINGS", "<", ">", "NAILS", "NAILS-PSEUDO", ")"))

//  def parse(tokens: Seq[String]): Tree = {
//
//     acc = (index, seq)
//
//    def parseWithAcc(acc: (Int, Seq[]))

//  }
}
