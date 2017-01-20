package models

case class Properties(props: Map[String, Value] = Map[String, Value]()) {
  def get(name: String): Option[String] = props.get(name) collect {
    case StringValue(s) => s
    case SeqValue(seq) => seq.mkString(" ")
  }
  def getInt(name: String): Option[Int] = props.get(name) collect {
    case IntValue(i) => i
  }
  def getSeq(name: String): Seq[String] = props.get(name) match {
    case Some(SeqValue(seq)) => seq
    case _ => Nil
  }

  def add(name: String, value: String): Properties = Properties(props + (name -> StringValue(value)))
  def addInt(name: String, value: Int): Properties = Properties(props + (name -> IntValue(value)))
  def addSeq(name: String, value: String): Properties = Properties(props + (name -> SeqValue(value +: getSeq(name))))
}

object PropertyName {
  val DESC = "DESC"
  val SYNONYM = "SYNONYM"
  val ADJECTIVE = "ADJECTIVE"
  val FDESC = "FDESC"
  val LDESC = "LDESC"
  val SIZE = "SIZE"
}