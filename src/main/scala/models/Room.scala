package models

import parsers.KeyWords


case class Room(id: Id,
                exits: Map[Direction, Exit] = Map.empty,
                action: Option[Action] = None,
                properties: Properties = new Properties,
                var flags: Seq[Flag] = Nil)
  extends HasId with HasLocation with HasFlags with HasProperties with ContainsObjects {

  val location = Rooms

  def north: Exit = exits.getOrElse(North, NoExit)
  def south: Exit = exits.getOrElse(South, NoExit)
  def west: Exit = exits.getOrElse(West, NoExit)
  def east: Exit = exits.getOrElse(East, NoExit)
  def up: Exit = exits.getOrElse(Up, NoExit)
  def down: Exit = exits.getOrElse(Down, NoExit)

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
    point2(PER, (r, dir, roomId) => Direction.unapply(dir).fold(r)(dir => r.withExit(dir, FExit(roomId)))),
    point(ACTION, (r, action) => r.withAction(Action(action))),
    points(FLAGS, (r, flag) => r.withFlag(Flag(flag))),
    // TODO global
    point(GLOBAL, (r, _) => r),
    // TODO things
    points(THINGS, (r, _) => r)
  ))
}