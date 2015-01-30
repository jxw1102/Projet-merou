package ast

import scala.collection.mutable.ArrayBuffer
import java.lang.Long.parseLong
import scala.collection.mutable.ArrayStack
import scala.io.Source
import scala.util.matching.Regex

import util.control.Breaks._

object ASTParser {
    implicit class ExtendedString(s: String) {
        val regex  = "\\w|(<<<)".r
        def indent = s.indexOf(regex.findFirstIn(s).get)
        def data   = s.substring(indent)
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
		
    	val idReg  = "(\\w+) (0x[\\da-f]{9})".r
        
        val stack  = ArrayStack[ASTNode]()
        val tree   = OtherASTNode(-1, "")
        stack.push(tree)
    	
        // break is usually discouraged in Scala (that's why it is not a keyword)
        // I propose you a more functional solution
        lines.map(line => (getCodeRange(line),idReg.findFirstMatchIn(line),line))
        	.filter(tuple => !tuple._2.isDefined || tuple._1.isDefined)
        	.foreach(tuple => {
        		val node = tuple match {
        			case (Some(codeRange),Some(m),line) =>
                		ConcreteASTNode(line.indent/2,m.group(1),parseLong(m.group(2).substring(2),16),codeRange,line.data)
        			case (None,None,line)    =>
                		line.data match {
                			case "<<<NULL>>>" => NullASTNode(line.indent/2)
                			case _            => OtherASTNode(line.indent/2,line.data)
                		}
        			case (_,_,line) => throw new ParseFailedException(line)
        		}	
            
        		while(node.depth <= stack.head.depth) stack.pop()
        		stack.head.children += node
        		stack.push(node)
    	})
        
//    	lines.foreach(line => {
//            breakable {
//                val idMatcher = idReg.findFirstMatchIn(line)
//                val data = line.substring(line.indent)
//                val node: ASTNode = idMatcher match {
//                    case Some(m) =>
//                        val codeRange = getCodeRange(line)
//                        if(codeRange == None) {
//                            break
//                        }
//                        ConcreteASTNode(line.indent/2,m.group(1),java.lang.Long.parseLong(m.group(2).substring(2),16),codeRange.get,data)
//                    case None    =>
//                        data match {
//                            case "<<<NULL>>>"    =>    NullASTNode(line.indent/2)
//                            case _               =>    OtherASTNode(line.indent/2,data)
//                        }
//                }
//                
//                while(node.depth <= stack.head.depth) stack.pop()
//                stack.head.children += node
//                stack.push(node)
//            }
//    	})
        println(tree.mkString);
    }
}

abstract class ASTNode(_depth: Int) {
    def depth    = _depth
    val children = ArrayBuffer[ASTNode]()
    
    // quality check : this is in my opinion a better way to print the tree
    def mkString = addString(new StringBuilder).toString
    def addString(sb: StringBuilder): StringBuilder = {
        sb.append("  " * depth + this + "\n")
        for (child <- children) child.addString(sb)
        sb
    }
}

object ASTNode {
//    To delete if you agree with my changes
///////////////////////////////////////////    
//	def displayChildren(t: ASTNode): Unit = {
//		for(n <- t.children) {
//			println("  " * n.depth + n)
//			displayChildren(n)
//		}
//	}
	
    def createAST(t: ASTNode): Unit = {
        var node = ProgramNode(t)
        // TODO
    }
}

case class ConcreteASTNode(_depth: Int, ofType: String, id: Long, pos: CodeRange, data: String) extends ASTNode(_depth) {
    def dataList = data.split(" ")
}

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


