package models

case class Room(id: Id, desc: Option[String] = None,
                exits: Map[Direction, Exit] = Map.empty,
                action: Option[Action] = None,
                flags: Seq[Flag] = Nil)
  extends HasId with HasLocation {

  val location = Rooms

  def north: Exit = exits.getOrElse(North, NoExit)
  def south: Exit = exits.getOrElse(South, NoExit)
  def west: Exit = exits.getOrElse(West, NoExit)
  def east: Exit = exits.getOrElse(East, NoExit)
  def up: Exit = exits.getOrElse(Up, NoExit)
  def down: Exit = exits.getOrElse(Down, NoExit)

  def withDesc(desc: String): Room = copy(desc = Some(desc))

  def withAction(action: Action): Room = copy(action = Some(action))

  def withExit(direction: Direction, exit: Exit): Room =
    copy(exits = exits + (direction -> exit))

  def withFlag(flag: Flag): Room = copy(flags = flags :+ flag)
}

object Room {
  import parsers.KeyWords._
  import parsers.ZParser._
  import parsers.ZParser

  val parser = ZParser[Room](zero(ROOM, Room(_)))(Seq(
    point(LOC, (r, _) => r),
    point(DESC, (r, desc) => r.withDesc(desc)),
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