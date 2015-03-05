package ast.model

import ast._
import graph.GraphNode

/**
 * Program contains the whole CFG in form of a map mapping the name of each global declaration
 * of the source code to the root node of its corresponding CFG.
 * @author David Courtinot 
 * @author Sofia Boutahar
 * @author Xiaowen Ji
 */
case class Program(val decls: Map[String,GraphNode[ProgramNode]]) {
                                                                   // node name cannot contain symbols
    override def toString = decls.map { case (k,v) => v.toDot(n => k.replaceAll("[^a-zA-Z0-9_]","_") + n.id,n => n.toString) }
                                 .addString(new StringBuilder)
                                 .toString
}

/**
 * Abstract type for the high-level representation of the source code
 */
abstract class SourceCodeNode {
    def apply[T <: SourceCodeNode](node: T, codeRange: CodeRange, id: String) = { node.codeRange = codeRange; node.id = id; node }
    
    private[this] var _codeRange: Option[CodeRange] = None
    private[this] var _id       : Option[String]    = None
    
    def codeRange                     = _codeRange
    def codeRange_=(range: CodeRange) = _codeRange = Some(range)
    
    def id               = _id
    def id_=(id: String) = _id = Some(id)
}

object SourceCodeNode {
    def apply[T <: SourceCodeNode](node: T, codeRange: CodeRange, id: String) = { node.codeRange = codeRange; node.id = id; node }
}

/**
 * ProgramNode is a very basic representation of the code. There is no notion of block and no relation between
 * different kind of nodes. It is meant to be the type of the values of a GraphNode representing a CFG.
 */
sealed abstract class ProgramNode(val id: String) {
    type SCN = SourceCodeNode
    
    override def equals(that: Any) = that match {
        case x: ProgramNode => id == x.id 
        case _              => false
    }
    override def hashCode = id.hashCode
    override def toString = {
        val format = (name: String, a: Any, id: String, cr: CodeRange) => "%s %s at %s %s".format(name,a,cr,id)
        this match {
            case If        (e   ,cr,id) => format("if"          ,e ,id,cr)
            case While     (e   ,cr,id) => format("while"       ,e ,id,cr)
            case Statement (stmt,cr,id) => format(stmt.toString ,"",id,cr)
            case Expression(e   ,cr,id) => format(e.toString,""    ,id,cr)
            case Switch    (e   ,cr,id) => format("switch"       ,e,id,cr)
            case For       (e   ,cr,id) => format("for"         ,if (e.isDefined) e.get else "no_cond",id,cr)
            case Empty     (_   ,   id) => format("Empty"       ,"",id,CodeRange(0,0,0,0))
        }
    }
}

// those classes will be the values of the CFG nodes
final case class If        (e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class For       (e: Option[Expr]     , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class While     (e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Statement (stmt: SourceCodeNode, cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Expression(e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Switch    (e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
// only used during the construction of the graph, should never be used in an actual CFG
private[ast] final case class Empty(              cr: CodeRange, _id: String) extends ProgramNode(_id)