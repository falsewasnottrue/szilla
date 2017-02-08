package models

case class Syntax(verbId: String, verb: String,
                  preAction: Option[String] = None,
                  obj1: Option[ObjectToken] = None,
                  prep: Option[String] = None,
                  obj2: Option[ObjectToken] = None)

object Syntax {
  val SYNTAX = """<SYNTAX (.+)=(.+)>""".r

  def unapply(str: String): Option[Syntax] = {
    str match {
      case SYNTAX(formPart, idPart) => {
        val (verbId, preAction) = idPart.trim.split(" ").toSeq.map(_.trim) match {
          case Seq(v) => (v, None)
          case Seq(v,p) => (v, Some(p))
          case _ => throw new IllegalArgumentException(s"illegal idPart $idPart")
        }

        val ps =  formPart.split(" ").toSeq.map(_.trim).foldLeft((Seq[String](), 0)) {
          case ((Nil, 0), curr) => (Seq(curr), 0)
          //  0 = Start
          case ((Seq(v), 0), curr) =>
              if (curr != "OBJECT") {
                (Seq(v + " " + curr), 0)
              } else {
                (Seq(v, curr), 1)
              }
          //  1 = Object gelesen
          case ((Seq(v,o1), 1), curr) =>
            if (curr.startsWith("(") && curr.endsWith(")")) {
              (Seq(v, o1 + " " + curr), 3)
            } else if (curr.startsWith("(")) {
              (Seq(v, o1 + " " + curr), 2)
            } else if (curr == "OBJECT") {
              (Seq(v, o1, "", curr), 4)
            } else {
              (Seq(v, o1, curr), 3)
            }
          //  2 = ( gelesen
          case ((Seq(v,o1), 2), curr) =>
            if (curr.endsWith(")")) {
              (Seq(v, o1 + " " + curr), 3)
            } else {
              (Seq(v, o1 + " " + curr), 2)
            }
          //  3 = ) gelesen oder keine (
          case ((Seq(v,o1), 3), curr) =>
            if (curr == "OBJECT") {
              (Seq(v,o1,"",curr), 4)
            } else {
              (Seq(v,o1,curr), 3)
            }
          case ((Seq(v,o1,p), 3), curr) =>
            if (curr == "OBJECT") {
              (Seq(v,o1,p,curr), 4)
            } else {
              (Seq(v,o1,p + " "+ curr), 3)
            }
          //  4 = Object gelesen
          case ((Seq(v,o1,p,o2), 4), curr) =>
            if (curr.startsWith("(")) {
              (Seq(v,o1,p,o2 + " " + curr), 5)
            } else {
              throw new IllegalStateException(s"trailing syntax element $curr")
            }
          //  5 = ( gelesen
          case ((Seq(v,o1,p,o2), 5), curr) =>
            (Seq(v,o1,p,o2 + " " + curr), 5)
        }._1

        ps match {
          case Seq(verb) => Some(Syntax(verbId, verb, preAction))
          case Seq(verb, obj1) => Some(Syntax(verbId, verb, preAction, obj1 = ObjectToken.unapply(obj1)))
          case Seq(verb, obj1, "", obj2) =>
            Some(Syntax(verbId, verb, preAction,
              obj1 = ObjectToken.unapply(obj1), prep = None, obj2 = ObjectToken.unapply(obj2)))
          case Seq(verb, obj1, prep, obj2) =>
            Some(Syntax(verbId, verb, preAction,
              obj1 = ObjectToken.unapply(obj1), prep = Some(prep), obj2 = ObjectToken.unapply(obj2)))
          case _ => throw new IllegalArgumentException(s"cannot parse syntax $ps")
        }
      }
      case _ => throw new IllegalArgumentException(s"not a syntax: $str")
    }
  }

}
