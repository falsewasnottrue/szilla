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
    private val contained = scala.collection.mutable.ListBuffer[Object]()

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

    def next(obj: Object): Option[Object] = ???
  }
}