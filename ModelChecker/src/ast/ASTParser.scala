package ast

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import scala.io.Source
import scala.util.matching.Regex

object ASTParser {
    implicit class ExtendedString(s: String) {
        val regex = "\\w|(<<<)".r
        def indent = s.indexOf(regex.findFirstIn(s).get)
    }

    var currentLine = 0
    val lineRangeReg = new Regex("line:(\\d+)(:(\\d+))?|col:(\\d+)", "line0", "", "line1", "col")
    
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

    def main(args: Array[String]) {
    	val lines  = Source.fromFile(args(0)).getLines.toSeq
    	val indent = lines(0).indent
		
    	val idReg = "(\\w+) (0x[\\da-f]{9})".r
        
        var stack = ArrayStack[ASTNode]()
        var tree = OtherASTNode(0, "")
        stack.push(tree)
    	
    	lines.takeWhile(_.indent >= indent).foreach(line => {
    		val idMatcher = idReg.findFirstMatchIn(line)
            val data = line.substring(line.indent)
    		val node: ASTNode = idMatcher match {
    		    case Some(m) =>
    				ConcreteASTNode(line.indent/2,m.group(1),java.lang.Long.parseLong(m.group(2).substring(2),16),getCodeRange(line).get,data)
    		    case None    =>
                    data match {
                        case "<<<NULL>>>"    =>    NullASTNode(line.indent/2)
                        case _               =>    OtherASTNode(line.indent/2,data)
                    }
            }
            while(node.depth <= stack.head.depth) {
                stack.pop()
            }
            stack.head.children += node
            stack.push(node)
    	})
        
        def displayChildren(t: ASTNode) : Unit = {
            for(n <- t.children) {
                println("  " * n.depth + n)
                displayChildren(n)
            }
        }
        
        println(tree)
        displayChildren(tree)
        
    }
}

abstract class ASTNode(_depth: Int) {
    def depth = _depth
    val children = ArrayBuffer[ASTNode]()
}
case class ConcreteASTNode(_depth: Int, ofType: String, id: Long, pos: CodeRange, data: String) extends ASTNode(_depth)
case class NullASTNode(_depth: Int) extends ASTNode(_depth)
case class OtherASTNode(_depth:Int, data: String) extends ASTNode(_depth)

case class CodeRange(lineMin: Int, lineMax: Int, colMin: Int, colMax: Int) {
    val lineRange = lineMin to lineMax
    val colRange  = colMin  to colMax
}

sealed abstract class CodePointer
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


