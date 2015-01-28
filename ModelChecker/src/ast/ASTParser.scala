package ast

import scala.io.Source
import scala.util.matching.Regex

object ASTParser extends App {
    implicit class ExtendedString(s: String) {
        val regex = "\\w".r
        def indent = s.indexOf(regex.findFirstIn(s).get)
    }

    def getCodeRange(line: String) = {
        val matcher = lineRangeReg.findAllIn(line)
        val l       = currentLine
        val res     = matcher.map(_ => CodePointer.parse(matcher.group(0))).toList match {
            case ColPointer (x)                       :: Nil => Some(CodeRange(l,l,x,x))
            case ColPointer (x)   :: ColPointer (y)   :: Nil => Some(CodeRange(l,l,x,y))
            case ColPointer (x)   :: LinePointer(y,z) :: Nil => Some(CodeRange(l,y,x,z))
            case LinePointer(v,w) :: LinePointer(x,y) :: Nil => Some(CodeRange(v,x,w,y))
            case LinePointer(x,y) :: ColPointer (z)   :: Nil => Some(CodeRange(x,x,y,z))
            case LinePointer(x,y)                     :: Nil => Some(CodeRange(x,x,y,y))
            case Nil                                         => None
            case _ => throw new ParseFailedException(line)
        }
        res match { case Some(x) => currentLine = x.lineMax; case None => }
        res
    }

    val lines  = Source.fromFile(args(0)).getLines.toSeq
    val indent = lines(0).indent

    var currentLine = 0
    val lineRangeReg = new Regex("line:(\\d+)(:(\\d+))?|col:(\\d+)", "line0", "", "line1", "col")
    val idReg = "(\\w+) (0x[\\da-f]{9})".r

    lines.takeWhile(_.indent >= indent).foreach(line => {
        val idMatcher = idReg.findFirstMatchIn(line)
        val a = idMatcher match {
            case Some(m) =>
                ASTNode(line.indent,m.group(1),java.lang.Long.parseLong(m.group(2).substring(2),16),getCodeRange(line).get)
            case None    =>
        }
        println(a)
    })
}


case class ASTNode(depth: Int, ofType: String, id: Long, pos: CodeRange)

case class CodeRange(lineMin: Int, lineMax: Int, colMin: Int, colMax: Int) {
    val lineRange = lineMin to lineMax
    val colRange  = colMin  to colMax
}

abstract class CodePointer
case class LinePointer(val line: Int, val col: Int) extends CodePointer
case class ColPointer(val col: Int) extends CodePointer

object CodePointer {
    val lineReg = new Regex("line:(\\d+)(:(\\d+))?", "line", "", "col")
    val colReg = new Regex("col:(\\d+)", "col")
    def parse(s: String) = {
        val line = lineReg.findAllIn(s)
        if (line.nonEmpty) {
            LinePointer(line.group("line").toInt, line.group("col").toInt)
        } else {
            val col = colReg.findFirstMatchIn(s)
            ColPointer(col.get.group("col").toInt)
        }
    }
}

class ParseFailedException(s: String) extends RuntimeException("Failed to parse : " + s)


