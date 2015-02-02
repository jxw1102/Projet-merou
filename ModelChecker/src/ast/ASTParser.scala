package ast

import java.lang.Long.parseLong

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import scala.io.Source
import scala.util.matching.Regex

/**
 * The ASTParser enables to parse the Clang AST file and return a tree data structure (ASTNode) with a very basic
 * identification of the different node of the original AST file. 
 * @author Xiaowen Ji 
 * @author David Courtinot
 */
object ASTParser {
    /**
     * Implicit declaration providing useful parsing methods on lines of the Clang AST file
     */
    implicit class ASTLine(s: String) {
        def indent    = s.indexOf(ASTLine.indentReg.findFirstIn(s).get)
        def data      = s.substring(indent)
        def id        = ASTLine.idReg.findFirstMatchIn(s)
        def codeRange = {
            val matcher = ASTLine.lineRangeReg.findAllIn(s)
                    val l   = currentLine
                    val res = matcher.map(_ => CodePointer.parse(matcher.group(0))).toList match {
                        case ColPointer (x)                       :: Nil => Some(CodeRange(l,l,x,x))
                        case ColPointer (x)   :: ColPointer (y)   :: Nil => Some(CodeRange(l,l,x,y))
                        case ColPointer (x)   :: LinePointer(y,z) :: Nil => Some(CodeRange(l,y,x,z))
                        case LinePointer(v,w) :: LinePointer(x,y) :: Nil => Some(CodeRange(v,x,w,y))
                        case LinePointer(x,y) :: ColPointer (z)   :: Nil => Some(CodeRange(x,x,y,z))
                        case LinePointer(x,y)                     :: Nil => Some(CodeRange(x,x,y,y))
                        case Nil                                         => None
                        case _ => throw new ParseFailedException(s)
            }
            res match { case Some(x) => currentLine = x.lineMax; case None => }
            res
        }
    }
    
    implicit object ASTLine {
        // the regex used to parse the file are compiled once and then used multiple times
        val indentReg    = "\\w|(<<<)".r
        val idReg        = "(\\w+) (0x[\\da-f]{9})".r
        val lineRangeReg = new Regex("line:(\\d+)(:(\\d+))?|col:(\\d+)", "line0", "", "line1", "col")
    }

    var currentLine = 0
    def main(args: Array[String]) {
        val lines  = Source.fromFile(args(0)).getLines.toSeq
        val stack  = ArrayStack[ASTNode]()
        val tree   = OtherASTNode(-1, "")
        stack.push(tree)
        
        lines.map(line => (line.codeRange,line.id,line.data,line.indent,line))
            .filter(tuple => !tuple._2.isDefined || tuple._1.isDefined)
            .foreach(tuple => {
                val node = tuple match {
                    case (Some(codeRange),Some(id),data,indent,_) =>
                        ConcreteASTNode(indent/2,id.group(1),parseLong(id.group(2).substring(2),16),codeRange,data)
                    case (None,None,data,indent,_)    =>
                        data match {
                            case "<<<NULL>>>" => NullASTNode(indent/2)
                            case _            => OtherASTNode(indent/2,data)
                        }
                    case (_,_,_,_,line) => throw new ParseFailedException(line)
                }    
            
                while(node.depth <= stack.head.depth) stack.pop()
                stack.head.children += node
                stack.push(node)
        })
        println(tree.mkString);
        
        /*
        def convert(t: ASTNode) : ProgramNode {
            if(t.children.isEmpty) {
                return ProgramNode(t)
            } else {
                val n = ProgramNode(t)
                foreach c in t.children => {
                    n.addStatement(convert(c))
                }
            }
        }
        */
        
        def unfoldExpr(t: ConcreteASTNode): Expr = {
            var node = t
            while (node.children.length > 0) {
              node = t.children.last.asInstanceOf[ConcreteASTNode]
            }
            val dataList = node.data.split(" ")
            node.ofType match {
                case "DeclRefExpr" => DeclRefExpr(dataList.last, dataList(dataList.length-2))
                case t if t.endsWith("Literal") => Litteral(dataList.last)
                case _             => throw new ConversionFailedException(node.mkString)
            }
        }
        
        def createBinaryOperator(t: ConcreteASTNode): BinaryOp = {
            val exprs: ArrayBuffer[Expr] = ArrayBuffer()
            t.children.foreach { c =>
                val node = c.asInstanceOf[ConcreteASTNode]
                val dataList = node.data.split(" ")
                node.ofType match {
                    case "ImplicitCastExpr" | "CStyleCastExpr" => exprs += unfoldExpr(node)
                    case t if t.endsWith("Literal") => exprs += Litteral(dataList.last)
                }
            }
            BinaryOp(exprs(0), exprs(1));
        }
        
        def createValDecl(t: ConcreteASTNode): VarDecl = {
            var expr: Option[Expr] = None
            try {
                t.children.last.asInstanceOf[ConcreteASTNode].ofType match {
                    case t if t.endsWith("Literal") => expr = Some(Litteral(t.data.split(" ").last))
                    case "BinaryOperator" => expr = None
                }
            } catch {
              case t: NoSuchElementException => t.printStackTrace()
            }
            val dataList = t.data.split(" ")
            VarDecl(dataList(dataList.length-2),Type(dataList.last),expr)
        }
        
        def createDeclStmt(t: ASTNode): DeclStmt = {
            val stmt = DeclStmt()
            t.children.foreach { c =>
                c match {
                    case ConcreteASTNode(depth,ofType,id,pos,data) => 
                        ofType match {
                            case "ValDecl" => stmt.decls += createValDecl(c.asInstanceOf[ConcreteASTNode])
                            case _         => throw new ConversionFailedException(c.mkString)
                        }
                    case _ => throw new ConversionFailedException(c.mkString)
                }
            }
            stmt
        }
        
    }
}

/**
 * Classes ASTNode, ConcreteASTNode, NullASTNode and OtherASTNode are used to
 * construct a tree structure from the Clang AST file
 */
sealed abstract class ASTNode(_depth: Int) {
    def depth    = _depth
    val children = ArrayBuffer[ASTNode]()
    
    def mkString = addString(new StringBuilder).toString
    def addString(sb: StringBuilder): StringBuilder = {
        sb.append("  " * depth + this + "\n")
        for (child <- children) child.addString(sb)
        sb
    }
}
final case class ConcreteASTNode(_depth: Int, ofType: String, id: Long, pos: CodeRange, data: String) extends ASTNode(_depth) {
    def dataList = data.split(" ")
}
final case class NullASTNode(_depth: Int) extends ASTNode(_depth)
final case class OtherASTNode(_depth:Int, data: String) extends ASTNode(_depth)

/**
 * Represents a piece of code between the lineMin:colMin and lineMax:colMax characters
 */
final case class CodeRange(lineMin: Int, lineMax: Int, colMin: Int, colMax: Int) {
    val lineRange = lineMin to lineMax
    val colRange  = colMin  to colMax
}

/**
 * Represents a single code pointer of the form line:i:j or col:j
 */
sealed abstract class CodePointer
final case class LinePointer(val line: Int, val col: Int) extends CodePointer
final case class ColPointer(val col: Int) extends CodePointer

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
class ConversionFailedException(s: String) extends RuntimeException("Failed to converte : " + s)
