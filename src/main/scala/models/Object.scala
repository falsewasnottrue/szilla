package models

import parsers.KeyWords

case class Object(
                 id: Id,
                 action: Option[Action] = None,
                 var location: Location = Empty,
                 properties: Properties = new Properties,
                 var flags: Seq[Flag] = Nil)
  extends HasId with HasLocation with HasFlags with HasProperties with ContainsObjects {

  def withLocation(location: Location) = copy(location = location)

  def withSynonym(synonym: String) = copy(properties = properties.addSeq(PropertyName.SYNONYM, synonym))

  def withAdjective(adjective: String) = copy(properties = properties.addSeq(PropertyName.ADJECTIVE, adjective))

  def withDesc(desc: String) = copy(properties = properties.add(PropertyName.DESC, desc))

  def withFlag(flag: Flag) = copy(flags = flags :+ flag)

  def withAction(action: Action) = copy(action = Some(action))

  def withFDesc(fdesc: String) = copy(properties = properties.add(PropertyName.FDESC, fdesc))

  def withLDesc(ldesc: String) = copy(properties = properties.add(PropertyName.LDESC, ldesc))

  def withSize(size: Int) = copy(properties = properties.addInt(PropertyName.SIZE, size))

  def synonyms: Seq[String] = properties.getSeq(PropertyName.SYNONYM)

  def adjectives: Seq[String] = properties.getSeq(PropertyName.ADJECTIVE)

  override def setLocation(location: Location): Unit = this.location = location

  override def addFlag(flag: Flag): Unit = this.flags = flags :+ flag

  override def removeFlag(flag: Flag): Unit = this.flags = flags.filter(_ != flag)
}

object Object {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Object](zero(OBJECT, Object(_)))(Seq(
      point(KeyWords.LOC, (o, id) => o.withLocation(RefLocation(id))),
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