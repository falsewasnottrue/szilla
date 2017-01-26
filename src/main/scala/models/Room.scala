package models

import parsers.{KeyWords, Leaf, Node}


case class Room(id: Id,
                exits: Map[Direction, Exit] = Map.empty,
                action: Option[Action] = None,
                properties: Properties = new Properties,
                var flags: Seq[Flag] = Nil)
  extends HasId with HasLocation with HasFlags with HasProperties with ContainsObjects {

  val location = Rooms

  def exit(dir: Direction) = exits.getOrElse(dir, NExit())

  def withDesc(desc: String): Room = copy(properties = properties.add(PropertyName.DESC, desc))

  def withAction(action: Action): Room = copy(action = Some(action))

  def withExit(direction: Direction, exit: Exit): Room =
    copy(exits = exits + (direction -> exit))

  def withFlag(flag: Flag): Room = copy(flags = flags :+ flag)

  override def setLocation(location: Location): Unit = throw new IllegalStateException(s"room's location cannot be changed")

  override def addFlag(flag: Flag): Unit = this.flags = flags :+ flag

  override def removeFlag(flag: Flag): Unit = this.flags = flags.filter(_ != flag)
}

object Room {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Room](zero(ROOM, Room(_)))(Seq(
    point(KeyWords.LOC, (r, _) => r),
    point(KeyWords.DESC, (r, desc) => r.withDesc(desc)),
    point2(TO, (r, dir, roomId) => Direction.unapply(dir).fold(r)(dir => r.withExit(dir, UExit(roomId)))),
    // conditional exit
    { case (r, Node(Seq(Leaf(dir), Leaf("TO"), Leaf(roomId), Leaf("IF"), Leaf(cond), Leaf("ELSE"), Leaf(otherwise)), _)) =>
      Direction.unapply(dir).fold(r)(dir => r.withExit(dir, CExit(roomId, Variable(cond), otherwise))) },
    // non-exit with message
    { case (r, Node(Seq(Leaf(dir), Leaf("SORRY"), Leaf(msg)), _)) =>
      Direction.unapply(dir).fold(r)(dir => r.withExit(dir, NExit(Some(msg)))) },
    point2(PER, (r, dir, roomId) => Direction.unapply(dir).fold(r)(dir => r.withExit(dir, FExit(roomId)))),
    point(ACTION, (r, action) => r.withAction(Action(action))),
    points(FLAGS, (r, flag) => r.withFlag(Flag(flag))),
    // TODO process global
    point(GLOBAL, (r, _) => r),
    // TODO process things
    points(THINGS, (r, _) => r)
  ))
}