package models

case class Object(
                 id: Id,
                 location: Location = Empty,
                 synonyms: Seq[String] = Nil,
                 adjectives: Seq[String] = Nil,
                 desc: Option[String] = None,
                 flags: Seq[Flag] = Nil,
                 action: Option[Action] = None,
                 fdesc: Option[String] = None,
                 ldesc: Option[String] = None,
                 size: Int = 0)
  extends HasId with HasLocation with Location {

  def withLocation(location: Location) = copy(location = location)

  def withSynonym(synonym: String) = copy(synonyms = synonyms :+ synonym)

  def withAdjective(adjective: String) = copy(adjectives = adjectives :+ adjective)

  def withDesc(desc: String) = copy(desc = Some(desc))

  def withFlag(flag: Flag) = copy(flags = flags :+ flag)

  def withAction(action: Action) = copy(action = Some(action))

  def withFDesc(fdesc: String) = copy(fdesc = Some(fdesc))

  def withLDesc(ldesc: String) = copy(ldesc = Some(ldesc))

  def withSize(size: Int) = copy(size = size)

  // TODO put into trait?
  // TODO test

  private val contained = scala.collection.mutable.ListBuffer[Object]()

  def contains(other: Object): Boolean = contained.contains(other)

  def insert(other: Object): Unit = contained.append(other)
}

object Object {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Object](zero(OBJECT, Object(_)))(Seq(
      point(LOC, (o, id) => o), // o.withLocation(RoomLocation(id))),
      points(SYNONYM, (o, synonym) => o.withSynonym(synonym)),
      points(ADJECTIVE, (o, adjective) => o.withAdjective(adjective)),
      point(DESC, (o, desc) => o.withDesc(desc)),
      points(FLAGS, (o, flag) => o.withFlag(Flag(flag))),
      point(ACTION, (o, action) => o.withAction(Action(action))),
      point(FDESC, (o, fdesc) => o.withFDesc(fdesc)),
      point(LDESC, (o, ldesc) => o.withLDesc(ldesc)),
      point(SIZE, (o, size) => o.withSize(size.toInt))
    )
  )
}