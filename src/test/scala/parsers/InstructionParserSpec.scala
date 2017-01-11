package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class InstructionParserSpec extends FlatSpec with Matchers {

  "Instruction parser" should "parse add operations" in {
    val res1 = Instruction.parser.parse("<ADD ,FRONT-SEAT-PASSENGERS ,BACK-SEAT-PASSENGERS>")
    res1.opcode should be(ADD)
    res1.operands should be(Seq(",FRONT-SEAT-PASSENGERS", ",BACK-SEAT-PASSENGERS"))

    val res2 = Instruction.parser.parse("<+ ,FRONT-SEAT-PASSENGERS ,BACK-SEAT-PASSENGERS>")
    res2.opcode should be(ADD)
    res2.operands should be(Seq(",FRONT-SEAT-PASSENGERS", ",BACK-SEAT-PASSENGERS"))
  }

  it should "parse sub operations" in {
    val res1 = Instruction.parser.parse("<SUB ,LT-BLATHER-ANGER 5>")
    res1.opcode should be(SUB)
    res1.operands should be(Seq(",LT-BLATHER-ANGER", "5"))

    val res2 = Instruction.parser.parse("<- ,LT-BLATHER-ANGER 5>")
    res2.opcode should be(SUB)
    res2.operands should be(Seq(",LT-BLATHER-ANGER", "5"))
  }

  it should "parse mul operations" in {
    val res1 = Instruction.parser.parse("<MUL ,MARTIANS-IN-ROOM ,ANTENNA-ON-A-MARTIAN>")
    res1.opcode should be(MUL)
    res1.operands should be(Seq(",MARTIANS-IN-ROOM", ",ANTENNA-ON-A-MARTIAN"))

    val res2 = Instruction.parser.parse("<* ,MARTIANS-IN-ROOM ,ANTENNA-ON-A-MARTIAN>")
    res2.opcode should be(MUL)
    res2.operands should be(Seq(",MARTIANS-IN-ROOM", ",ANTENNA-ON-A-MARTIAN"))
  }

  it should "parse div operations" in {
    val res1 = Instruction.parser.parse("<DIV ,SCREEN-WIDTH 2>")
    res1.opcode should be(DIV)
    res1.operands should be(Seq(",SCREEN-WIDTH", "2"))

    val res2 = Instruction.parser.parse("</ ,SCREEN-WIDTH 2>")
    res2.opcode should be(DIV)
    res2.operands should be(Seq(",SCREEN-WIDTH", "2"))
  }

  it should "parse mod operations" in {
    val res1 = Instruction.parser.parse("<MOD ,PEBBLES-IN-PILE 10>")
    res1.opcode should be(MOD)
    res1.operands should be(Seq(",PEBBLES-IN-PILE", "10"))

    val res2 = Instruction.parser.parse("<MOD ,PEBBLES-IN-PILE 10>")
    res2.opcode should be(MOD)
    res2.operands should be(Seq(",PEBBLES-IN-PILE", "10"))
  }

  it should "parse random operations" in {
    val res1 = Instruction.parser.parse("<RANDOM 17>")
    res1.opcode should be(RANDOM)
    res1.operands should be(Seq("17"))

    val res2 = Instruction.parser.parse("<RANDOM 17>")
    res2.opcode should be(RANDOM)
    res2.operands should be(Seq("17"))
  }

  // ...
}
