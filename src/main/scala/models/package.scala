package object models {
  type Id = String

  trait HasId {
    def id: Id
  }

  trait HasLocation {
    def location: Location
  }
}