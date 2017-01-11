package models

case class Object(
                 id: Id, location: Location,
                 synonyms: Seq[String] = Nil,
                 adjectives: Seq[String] = Nil,
                 desc: Option[String] = None,
                 flags: Seq[Flag] = Nil,
                 action: Option[Action] = None,
                 fdesc: Option[String] = None,
                 ldesc: Option[String] = None,
                 size: Int = 0)
  extends HasId with HasLocation {

  def withSynonym(synonym: String) = copy(synonyms = synonyms :+ synonym)

  def withAdjective(adjective: String) = copy(adjectives = adjectives :+ adjective)

  def withDesc(desc: String) = copy(desc = Some(desc))

  def withFlag(flag: Flag) = copy(flags = flags :+ flag)

  def withAction(action: Action) = copy(action = Some(action))

  def withFDesc(fdesc: String) = copy(fdesc = Some(fdesc))

  def withLDesc(ldesc: String) = copy(ldesc = Some(ldesc))

  def withSize(size: Int) = copy(size = size)
}
