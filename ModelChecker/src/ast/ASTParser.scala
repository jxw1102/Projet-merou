package ast

import java.lang.Long.parseLong

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.io.Source
import scala.util.matching.Regex

/**
 * The ASTParser enables to parse the Clang AST file and return a tree data structure (ASTNode) with a very basic
 * identification of the different node of the original AST file. 
 * @author Xiaowen Ji 
 * @author Sofia Boutahar
 * @author David Courtinot
 */
class ASTParser {
    /// Implicit declaration providing useful parsing methods on lines of the Clang AST file
    private implicit class ASTLine(s: String) {
        def indent    = s.indexOf(ASTLine.indentReg.findFirstIn(s).get)
        def id        = ASTLine.idReg.findFirstMatchIn(s)
        def data      = s.substring(indent)
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
    
    private implicit object ASTLine {
        // the regex used to parse the file are compiled once and then used multiple times
        val indentReg    = "\\w|(<<<)".r
        val idReg        = "(\\w+) (0x[\\da-f]+)".r
        val lineRangeReg = new Regex("line:(\\d+)(:(\\d+))?[,>]|col:(\\d+)[,>]", "line0", "", "line1", "col")
    }
    
    private var currentLine = 1
    def parseFile(path: String) = {
        currentLine   = 0
        // skip first lines
        val lines  = Source.fromFile(path).getLines.dropWhile(_ contains "<invalid sloc>").toSeq
        val tree   = OtherASTNode(-1, "")
        val stack  = ArrayStack[ASTNode]()
        val labels = HashMap[String,String]()
        stack.push(tree)

        lines.map(line => (line.codeRange,line.id,line.data,line.indent,line))
            .filter(tuple => !tuple._2.isDefined || tuple._1.isDefined)
            .foreach(tuple => {
                val node = tuple match {
                    case (Some(codeRange),Some(id),data,indent,_) =>
                        val cnode = ConcreteASTNode(indent/2,id.group(1),id.group(2),codeRange,data)
                        id.group(1) match {
                            case "LabelStmt" => labels += cnode.id -> cnode.data.dataList.last; cnode
                            case x if x.endsWith("Stmt") => cnode
                            case x if x.endsWith("Expr") => cnode
                            case x if x.endsWith("Operator") => cnode
                            case x if x.contains("Literal") => cnode
                            case "VarDecl" | "FunctionDecl" | "ParmVarDecl"
//                                | "CXXRecordDecl" | "CXXConstructorDecl" | "CXXDestructorDecl" | "CXXMethodDecl" | "CXXCtorInitializer"
                                | "TypedefDecl" | "FieldDecl" | "EnumDecl" | "EnumConstantDecl" => cnode
                            case _           => OtherASTNode(indent/2,data)
                        }
                    case (None,None,data,indent,_) =>
                        data match {
                            case "<<<NULL>>>" => NullASTNode(indent/2)
                            case _            => OtherASTNode(indent/2,data)
                        }
                    case (_,_,_,_,line) => throw new ParseFailedException(line)
                }
                
                while (node.depth <= stack.head.depth) stack.pop
                stack.head.children += node
                stack.push(node)
        })
        
        val children = tree.children.filterNot(_.isInstanceOf[OtherASTNode])
        tree.children.clear()
        tree.children ++= children
        
        new ASTParserResult(tree,labels)
    }
}

/**
 * Contains the result of the parsing : the root node, always to be ignored, and a map mapping 
 * the labels' id to the id of the node they point to.
 */
final class ASTParserResult(val root: ASTNode, val labels: Map[String,String])

/**
 * Abstract class representing the tree data-structure of the AST
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
/**
 * Represents all the nodes in the AST which correspond to a Clang class. 
 */
final case class ConcreteASTNode(_depth: Int, ofType: String, id: String, pos: CodeRange, data: String) extends ASTNode(_depth) {
    override def equals(that: Any) = that match { case ConcreteASTNode(_,_,_id,_,_) => id == _id; case _ => false }
    override def hashCode          = id.hashCode
}
/**
 * Represents Clang <<<NULL>>> nodes.
 */
final case class NullASTNode(_depth: Int) extends ASTNode(_depth)
/**
 * Represents any node other than ConcreteASTNode or NullASTNode. It is meant to avoid parsing failures in case the AST
 * contains some kinds of nodes we did not handle. It is up to the SourceCodeNodeFactory to know how to handle such nodes. 
 */
final case class OtherASTNode(_depth:Int, data: String) extends ASTNode(_depth)

/**
 * Represents a piece of code between the lineMin:colMin and lineMax:colMax characters.
 * @pre 0 < lineMin < lineMax
 * @pre 0 < colMin < colMax
 */
final case class CodeRange(lineMin: Int, lineMax: Int, colMin: Int, colMax: Int) {
    val lineRange = lineMin to lineMax
    val colRange  = colMin  to colMax
    override def toString = if (lineMin == lineMax) "(%d,%d:%d)".format(lineMin,colMin,colMax) else "(%d:%d,%d:%d)".format(lineMin,lineMax,colMin,colMax)
}

/**
 * Represents a single source code pointer of the form line:i:j or col:j.
 * @pre i > 0
 * @pre j > 0
 */
sealed abstract class CodePointer
final case class LinePointer(line: Int, col: Int) extends CodePointer
final case class ColPointer(col: Int)             extends CodePointer

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

/**
 * Thrown when an unexpected error occurs during the parsing.
 */
class ParseFailedException     (s: String) extends RuntimeException("Failed to parse : " + s)
/**
 * Thrown when an unexpected error occurs during the conversion from ASTNode to SourceCodeNode.
 */
class ConversionFailedException(s: String) extends RuntimeException("Failed to convert: " + s)
