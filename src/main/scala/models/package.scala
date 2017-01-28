package object models {
  type Id = String

  trait HasId {
    def id: Id
  }

  trait HasLocation {
    def location: Location

    def setLocation(location: Location): Unit
  }

  trait ContainsObjects extends HasId {
    val contained = scala.collection.mutable.ListBuffer[Object]()

    def contains(other: Object): Boolean = contained.contains(other)

    def insert(other: Object): Unit = {
      contained.append(other)
      other.setLocation(RefLocation(id))
    }

    def remove(other: Object): Unit = {
      contained.remove(contained.indexOf(other))
      other.setLocation(Empty)
    }

    def first: Option[Object] = contained.headOption

    def next(obj: HasLocation): Option[Object] = findNext(contained, obj)

    private def findNext(rest: Seq[Object], obj: HasLocation): Option[Object] = rest match {
        case Nil => None
        case `obj` +: Nil => None
        case `obj` +: next +: _ => Some(next)
        case _ +: os => findNext(os, obj)
      }

  }

  trait HasFlags {
    def flags: Seq[Flag]

    def addFlag(flag: Flag): Unit

    def removeFlag(flag: Flag): Unit
  }

  trait HasProperties {
    def properties: Properties
  }
}
