package parsers

import models._
import org.scalatest.{FlatSpec, Matchers}

class InstructionParserSpec extends FlatSpec with Matchers {

  "Instruction parser" should "parse add operations" in {
    val res1 = Instruction.parser.parse("<ADD ,FRONT-SEAT-PASSENGERS ,BACK-SEAT-PASSENGERS>")
    res1.opcode should be(ADD)
    res1.operands should be(Seq(Variable(",FRONT-SEAT-PASSENGERS"), Variable(",BACK-SEAT-PASSENGERS")))
  }

  it should "parse + operations" in {
    val res2 = Instruction.parser.parse("<+ ,FRONT-SEAT-PASSENGERS ,BACK-SEAT-PASSENGERS>")
    res2.opcode should be(ADD)
    res2.operands should be(Seq(Variable(",FRONT-SEAT-PASSENGERS"), Variable(",BACK-SEAT-PASSENGERS")))
  }

  it should "parse sub operations" in {
    val res1 = Instruction.parser.parse("<SUB ,LT-BLATHER-ANGER 5>")
    res1.opcode should be(SUB)
    res1.operands should be(Seq(Variable(",LT-BLATHER-ANGER"), Variable("5")))
  }

  it should "parse - operations" in {
    val res2 = Instruction.parser.parse("<- ,LT-BLATHER-ANGER 5>")
    res2.opcode should be(SUB)
    res2.operands should be(Seq(Variable(",LT-BLATHER-ANGER"), Variable("5")))
  }

  it should "parse mul operations" in {
    val res1 = Instruction.parser.parse("<MUL ,MARTIANS-IN-ROOM ,ANTENNA-ON-A-MARTIAN>")
    res1.opcode should be(MUL)
    res1.operands should be(Seq(Variable(",MARTIANS-IN-ROOM"), Variable(",ANTENNA-ON-A-MARTIAN")))
  }

  it should "parse * operations" in {
    val res2 = Instruction.parser.parse("<* ,MARTIANS-IN-ROOM ,ANTENNA-ON-A-MARTIAN>")
    res2.opcode should be(MUL)
    res2.operands should be(Seq(Variable(",MARTIANS-IN-ROOM"), Variable(",ANTENNA-ON-A-MARTIAN")))
  }

  it should "parse div operations" in {
    val res1 = Instruction.parser.parse("<DIV ,SCREEN-WIDTH 2>")
    res1.opcode should be(DIV)
    res1.operands should be(Seq(Variable(",SCREEN-WIDTH"), Variable("2")))
  }

  it should "parse / operations" in {
    val res2 = Instruction.parser.parse("</ ,SCREEN-WIDTH 2>")
    res2.opcode should be(DIV)
    res2.operands should be(Seq(Variable(",SCREEN-WIDTH"), Variable("2")))
  }

  it should "parse mod operations" in {
    val res1 = Instruction.parser.parse("<MOD ,PEBBLES-IN-PILE 10>")
    res1.opcode should be(MOD)
    res1.operands should be(Seq(Variable(",PEBBLES-IN-PILE"), Variable("10")))
  }

  it should "parse random operations" in {
    val res1 = Instruction.parser.parse("<RANDOM 17>")
    res1.opcode should be(RANDOM)
    res1.operands should be(Seq(Variable("17")))
  }

  it should "parse equal? operations" in {
    val text = "<EQUAL? ,HERE ,OVAL-OFFICE ,ROSE-GARDEN ,PORTICO>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(EQUAL_Q)
    res.operands should be(Seq(Variable(",HERE"), Variable(",OVAL-OFFICE"), Variable(",ROSE-GARDEN"), Variable(",PORTICO")))
  }

  it should "parse zero? operations" in {
    val text = "<ZERO? ,FUEL-LEVEL>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(ZERO_Q)
    res.operands should be(Seq(Variable(",FUEL-LEVEL")))
  }

  it should "parse less? operations" in {
    val text = "<LESS? ,AIR-PRESSURE 4>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(LESS_Q)
    res.operands should be(Seq(Variable(",AIR-PRESSURE"), Variable("4")))
  }

  it should "parse l? operations" in {
    val text = "<L? ,AIR-PRESSURE 4>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(LESS_Q)
    res.operands should be(Seq(Variable(",AIR-PRESSURE"), Variable("4")))
  }

  it should "parse grtr? operations" in {
    val text = "<GRTR? ,GONDOLA-WEIGHT ,BALLOON-LIFTING-CAPACITY>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(GRTR_Q)
    res.operands should be(Seq(Variable(",GONDOLA-WEIGHT"), Variable(",BALLOON-LIFTING-CAPACITY")))
  }

  it should "parse g? operations" in {
    val text = "<G? ,GONDOLA-WEIGHT ,BALLOON-LIFTING-CAPACITY>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(GRTR_Q)
    res.operands should be(Seq(Variable(",GONDOLA-WEIGHT"), Variable(",BALLOON-LIFTING-CAPACITY")))
  }

  it should "parse fset? operations" in {
    val text = "<FSET? ,BRASS-LAMP ,ONBIT>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(FSET_Q)
    res.operands should be(Seq(Variable(",BRASS-LAMP"), Variable(",ONBIT")))
  }

  it should "parse in? operations" in {
    val text = "<IN? ,SECRET-WILL ,WALL-SAFE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(IN_Q)
    res.operands should be(Seq(Variable(",SECRET-WILL"), Variable(",WALL-SAFE")))
  }

  it should "parse move operations" in {
    val text = "<MOVE ,BREAD ,TOASTER>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(MOVE)
    res.operands should be(Seq(Variable(",BREAD"), Variable(",TOASTER")))
  }

  it should "parse remove operations" in {
    val text = "<REMOVE ,ICE-CUBE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(REMOVE)
    res.operands should be(Seq(Variable(",ICE-CUBE")))
  }

  it should "parse loc operations" in {
    val text = "<LOC ,SMOKING-GUN>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(LOC)
    res.operands should be(Seq(Variable(",SMOKING-GUN")))
  }

  it should "parse first? operations" in {
    val text = "<FIRST? ,REFRIGERATOR>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(FIRST_Q)
    res.operands should be(Seq(Variable(",REFRIGERATOR")))
  }

  it should "parse next? operations" in {
    val text = "<NEXT? ,MAYONNAISE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(NEXT_Q)
    res.operands should be(Seq(Variable(",MAYONNAISE")))
  }

  it should "parse fset operations" in {
    val text = "<FSET ,OILY-TORCH ,FLAMEBIT>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(FSET)
    res.operands should be(Seq(Variable(",OILY-TORCH"), Variable(",FLAMEBIT")))
  }

  it should "parse fclear operations" in {
    val text = "<FCLEAR ,GUARDED-DIAMOND ,TRYTAKEBIT>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(FCLEAR)
    res.operands should be(Seq(Variable(",GUARDED-DIAMOND"), Variable(",TRYTAKEBIT")))
  }

  it should "parse getp operations" in {
    val text = "<GETP ,HERE ,P?LDESC>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(GETP)
    res.operands should be(Seq(Variable(",HERE"), Variable(",P?LDESC")))
  }

  it should "parse putp operations" in {
    val text = "<PUTP ,ROTTING-TOMATO ,P?SDESC \"rotten tomato\">"
    val res = Instruction.parser.parse(text)
    res.opcode should be(PUTP)
    res.operands should be(Seq(Variable(",ROTTING-TOMATO"), Variable(",P?SDESC"), Variable("rotten tomato")))
  }

  //

  it should "parse get operations" in {
    val text = "<GET ,LATITUDE-TABLE 30>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(GET)
    res.operands should be(Seq(Variable(",LATITUDE-TABLE"), Variable("30")))
  }

  it should "parse put operations" in {
    val text = "<PUT ,SUSPECTS-TABLE ,SUSPECTS-POINTER ,BUTLER>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(PUT)
    res.operands should be(Seq(Variable(",SUSPECTS-TABLE"), Variable(",SUSPECTS-POINTER"), Variable(",BUTLER")))
  }

  it should "parse intbl? operations" in {
    val text = "<INTBL? ,RUDOLPH ,REINDEER-TABLE 8>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(INTBL_Q)
    res.operands should be(Seq(Variable(",RUDOLPH"), Variable(",REINDEER-TABLE"), Variable("8")))
  }

  it should "parse copyt operations" in {
    val text = "<COPYT ,CURRENT-MOVE-TBL ,OLD-MOVE-TBL ,MOVE-TBL-LEN>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(COPYT)
    res.operands should be(Seq(Variable(",CURRENT-MOVE-TBL"), Variable(",OLD-MOVE-TBL"), Variable(",MOVE-TBL-LEN")))
  }

  it should "parse read operations" in {
    val text = "<READ ,P-INBUF-TBL>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(READ)
    res.operands should be(Seq(Variable(",P-INBUF-TBL")))
  }

  it should "parse input operations" in {
    val text = "<INPUT 1>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(INPUT)
    res.operands should be(Seq(Variable("1")))
  }

  it should "parse mouse-info operations" in {
    val text = "<MOUSE-INFO ,MOUSE-INFO-TBL>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(MOUSE_INFO)
    res.operands should be(Seq(Variable(",MOUSE-INFO-TBL")))
  }

  it should "parse mouse-limit operations" in {
    val text = "<MOUSE-LIMIT 0>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(MOUSE_LIMIT)
    res.operands should be(Seq(Variable("0")))
  }

  it should "parse menu operations" in {
    val text = "<MENU 3 ,BATTLE-COMMANDS-TBL>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(MENU)
    res.operands should be(Seq(Variable("3"), Variable(",BATTLE-COMMANDS-TBL")))
  }

  // pictures and sound

  it should "parse display operations" in {
    val text = "<DISPLAY ,P-TITLE 1 1>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(DISPLAY)
    res.operands should be(Seq(Variable(",P-TITLE"), Variable("1"), Variable("1")))
  }

  it should "parse picinf operations" in {
    val text = "<PICINF ,WATERFALL-PIC ,PICINF-TBL>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(PICINF)
    res.operands should be(Seq(Variable(",WATERFALL-PIC"), Variable(",PICINF-TBL")))
  }

  it should "parse sound operations" in {
    val text = "<SOUND ,CAR-BACKFIRE 2 5 2>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(SOUND)
    res.operands should be(Seq(Variable(",CAR-BACKFIRE"), Variable("2"), Variable("5"), Variable("2")))
  }

  // control operations

  it should "parse call operations" in {
    val text = "<CALL routine1 arg1 arg2 arg3>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(CALL)
    res.operands should be(Seq(Variable("routine1"), Variable("arg1"), Variable("arg2"), Variable("arg3")))
  }

  it should "parse return operations" in {
    val text = "<RETURN .COUNT>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(RETURN)
    res.operands should be(Seq(Variable(".COUNT")))
  }

  it should "parse rtrue operations" in {
    val text = "<RTRUE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(RTRUE)
    res.operands should be(Nil)
  }

  it should "parse rfalse operations" in {
    val text = "<RFALSE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(RFALSE)
    res.operands should be(Nil)
  }

  // game operations

  it should "parse quit operations" in {
    val text = "<QUIT>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(QUIT)
    res.operands should be(Nil)
  }

  it should "parse restart operations" in {
    val text = "<RESTART>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(RESTART)
    res.operands should be(Nil)
  }

  it should "parse verify operations" in {
    val text = "<VERIFY>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(VERIFY)
    res.operands should be(Nil)
  }

  it should "parse save operations" in {
    val text = "<SAVE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(SAVE)
    res.operands should be(Nil)
  }

  it should "parse restore operations" in {
    val text = "<RESTORE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(RESTORE)
    res.operands should be(Nil)
  }

  it should "parse isave operations" in {
    val text = "<ISAVE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(ISAVE)
    res.operands should be(Nil)
  }

  it should "parse irestore operations" in {
    val text = "<IRESTORE>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(IRESTORE)
    res.operands should be(Nil)
  }

  // special

  it should "parse setg operations" in {
    val text = "<SETG SCORE <+ ,SCORE .NUM>>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(SETG)

    res.operands should be(Seq(
      Variable("SCORE"),
      Instruction(ADD, Seq(Variable(",SCORE"), Variable(".NUM")))
    ))
  }

  it should "parse tell operations" in {
    val text = "<TELL \"[Your score has just gone up by \" N .NUM \".]\" CR>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(TELL)
    res.operands should be(Seq(
      Variable("[Your score has just gone up by "),
      Variable("N"),
      Variable(".NUM"),
      Variable(".]"),
      Variable("CR")
    ))
  }

  it should "parse cond operations" in {
    val text = "<COND (,SCORE-NOTIFICATION-ON <TELL \"[Your score has just gone up by \" N .NUM \".]\" CR>)>"
    val res = Instruction.parser.parse(text)
    res.opcode should be(COND)

    res.operands should be(Seq(
      Condition(
        Variable(",SCORE-NOTIFICATION-ON"),
        Instruction(TELL, Seq(
          Variable("[Your score has just gone up by "),
          Variable("N"),
          Variable(".NUM"),
          Variable(".]"),
          Variable("CR")
        ))
      )
    ))
  }
}
