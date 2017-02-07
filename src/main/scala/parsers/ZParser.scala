package parsers

case class ZParser[T](init: PartialFunction[ParseTree,(T, Seq[ParseTree])])(clauseParsers: Seq[PartialFunction[(T, ParseTree) ,T]]) {

  def parse(text: String): T = {
    val tokens = Tokenizer.tokenize(text)
    val tree = Parser.parse(tokens)

    parse(tree: ParseTree)
  }

  def parse(tree: ParseTree): T = {
    def enrich(t: T, clause: ParseTree): T = clauseParsers.
      find(parser => parser.isDefinedAt((t, clause))).
      map(parser => parser.apply((t, clause))).
      getOrElse(throw new IllegalArgumentException(s"no applicable parser found for clause $clause"))

    if (init.isDefinedAt(tree)) {
      val (start, clauses) = init(tree)
      clauses.foldLeft(start) {
        case (step, clause) => enrich(step, clause)
      }
    } else {
      throw new IllegalArgumentException(s"illegal text: $tree")
    }
  }
}

object ZParser {

  def zero0[T](keyword: String, make: => T): PartialFunction[ParseTree, (T, Seq[ParseTree])] =
  { case Node(Leaf(`keyword`) :: clauses, _) => (make, clauses) }

  def zero[T](keyword: String, make: String => T): PartialFunction[ParseTree, (T, Seq[ParseTree])] =
    { case Node(Leaf(`keyword`) :: Leaf(id) :: clauses, _) => (make(id), clauses) }

  def point[T](keyword: String, step: (T, String) => T): PartialFunction[(T, ParseTree), T] =
    { case (t, Node(Seq(Leaf(`keyword`), Leaf(id)), _)) => step(t, id) }

  def point2[T](keyword: String, step: (T, String, String) => T): PartialFunction[(T, ParseTree), T] =
  { case (t, Node(Seq(Leaf(dir), Leaf(`keyword`), Leaf(id)), _)) => step(t, dir, id) }

  def points[T](keyword: String, step: (T, String) => T): PartialFunction[(T, ParseTree), T] =
    {case (obj, Node(Leaf(`keyword`) +: clauses, _)) => clauses.foldLeft(obj) {
      case (o, Leaf(clause)) => step(o, clause)
      case (o, Node(_, _)) => o // ignore TODO really?
      case s => throw new IllegalArgumentException(s"illegal clause $keyword: $s")
    }}
}
