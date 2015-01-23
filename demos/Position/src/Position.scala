import scala.io.Source
import scala.util.matching.Regex

/*
 *  Extract all position values (ex: <line:20:1, col:2>) in AST
 *   
 * */

object Main {
  def main(args: Array[String]): Unit = {
    val lineRangeReg = new Regex("line:(\\d+)(:(\\d+))?|col:(\\d+)", "line0", "", "line1", "col")
    val lines = Source.fromFile(args(0)).getLines.dropWhile(!_.contains("main"))
    var currentLine = 0
    for (line <- lines) {
      val matcher = lineRangeReg.findAllIn(line)
      val test = matcher.map(_ => CodePointer.parse(matcher.group(0))).toList match {
        case ColPointer(x)                          :: Nil => CodeRange(currentLine, currentLine, x, x)        
        case LinePointer(x, y)                      :: Nil => currentLine = x; CodeRange(x, x, y, y)        
        case ColPointer(x)     :: ColPointer(y)     :: Nil => CodeRange(currentLine, currentLine, x, y)        
        case LinePointer(x, y) :: ColPointer(z)     :: Nil => currentLine = x; CodeRange(x, x, y, z)
        case LinePointer(v, w) :: LinePointer(x, y) :: Nil => currentLine = x; CodeRange(v, x, w, y)
        case ColPointer(x)     :: LinePointer(y, z) :: Nil => val res = CodeRange(currentLine, y, x, z)
                                                              currentLine = y
                                                              res          
        case Nil =>
        case _   => throw new ParseFailedException(line)
      }
      println(test)
    }
  }
}

class ParseFailedException(s: String) extends RuntimeException("Failed to parse : " + s)

case class CodeRange(lineMin: Int, lineMax: Int, colMin: Int, colMax: Int) {
  val lineRange = lineMin to lineMax
  val colRange = colMin to colMax
}

abstract class CodePointer
case class LinePointer(val line: Int, val col: Int) extends CodePointer
case class ColPointer(val col: Int) extends CodePointer

object CodePointer {
  val lineReg = new Regex("line:(\\d+)(:(\\d+))?", "line", "", "col")
  val colReg = new Regex("col:(\\d+)", "coli")
  def parse(s: String) = {
    val line = lineReg.findAllIn(s)
    if (line.nonEmpty) {
      LinePointer(line.group("line").toInt, line.group("col").toInt)
    } else {
      val col = colReg.findFirstMatchIn(s)
      ColPointer(col.get.group("coli").toInt)
    }
  }
}