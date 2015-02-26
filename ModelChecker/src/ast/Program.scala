package ast

import ast.model.Decl
import ast.model.Expr
import cfg.CFGDecl
import cfg.CFGExpr
import cfg.CFGVal
import ctl.GraphNode
import ast.model.CompoundAssignOp
import ast.model.UnaryOp
import ast.model.BinaryOp
import ast.model.ConditionalOperator
import ast.model.ArraySubscriptExpr
import ast.model.CallExpr
import ast.model.InitListExpr
import ast.model.VarDecl
import ast.model.DeclRefExpr

/**
 * Those classes represent the most abstract and final form of the transformations of the source code
 * from AST just before the CFG conversion.
 * @author David Courtinot 
 * @author Sofia Boutahar
 * @author Xiaowen Ji      
 */

case class Program(val decls: Map[String,GraphNode[ProgramNode]]) {
    override def toString = decls.values.map(_.mkString).addString(new StringBuilder).toString
}

/**
 * Abstract type for the high-level representation of the source code
 */
abstract class SourceCodeNode {
    private[this] var _codeRange: Option[CodeRange] = None
    private[this] var _id       : Option[String]    = None
    
    def codeRange                     = _codeRange
    def codeRange_=(range: CodeRange) = _codeRange = Some(range)
    
    def id               = _id
    def id_=(id: String) = _id = Some(id)
}

object SourceCodeNode {
    def apply(node: SourceCodeNode, codeRange: CodeRange, id: String) = { node.codeRange = codeRange; node.id = id; node }
}

sealed abstract class ProgramNode(val id: String) {
    type SCN = SourceCodeNode
    
    override def equals(that: Any) = that match {
        case x: ProgramNode => id == x.id 
        case _              => false
    }
    override def hashCode = id.hashCode
    override def toString = {
        val format = (name: String, a: Any, id: String, cr: CodeRange) => "\"%s %s at %s %s\"".format(name,a,cr,id)
        this match {
            case If        (e   ,cr,id) => format("if"          ,e ,id,cr)
            case While     (e   ,cr,id) => format("while"       ,e ,id,cr)
            case Statement (stmt,cr,id) => format(stmt.toString ,"",id,cr)
            case Identifier(s   ,cr,id) => format("Identifier"  ,s ,id,cr)
            case Expression(e   ,cr,id) => format(e.toString,""    ,id,cr)
            case Switch    (e   ,cr,id) => format("switch"       ,e,id,cr)
            case For       (e   ,cr,id) => format("for"         ,if (e.isDefined) e.get else "no_cond",id,cr)
            case Empty     (_   ,   id) => format("Empty"       ,"",id,CodeRange(0,0,0,0))
        }
    }
}

/**
 * Case-classes that will be the values of the CFG nodes
 */
final case class If        (e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class For       (e: Option[Expr]     , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class While     (e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Statement (stmt: SourceCodeNode, cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Identifier(s: String           , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Expression(e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
final case class Switch    (e: Expr             , cr: CodeRange, _id: String) extends ProgramNode(_id)
// only used during the construction of the graph, should never be used in an actual CFG
private[ast] final case class Empty(              cr: CodeRange, _id: String) extends ProgramNode(_id)

class CFGNode(value: ProgramNode) extends GraphNode[ProgramNode](value) {
    override def equals(that: Any) = that match { case x: CFGNode => value == x.value case _ => false }
    override def hashCode          = value.hashCode
}
