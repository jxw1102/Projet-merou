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
 * Program contains the whole CFG in form of a map mapping the name of each global declaration
 * of the source code to the root node of its corresponding CFG.
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

/**
 * ProgramNode is a very basic representation of the code. There is no notion of block and no relation between
 * different kind of nodes. It is meant to be the type of the values of a GraphNode representing a CFG.
 */
sealed abstract class ProgramNode(val id: String, val isCopy: Boolean) {
    type SCN = SourceCodeNode
    
    override def equals(that: Any) = that match {
        case x: ProgramNode => id == x.id 
        case _              => false
    }
    override def hashCode = id.hashCode
    override def toString = {
        val pref = if(isCopy) "idc" else "id"
        val format = (name: String, a: Any, id: String, cr: CodeRange) => "{%s [label=\"%s %s at %s\"]}".format(pref.concat(id),name,a,cr)
        this match {
            case If        (e   ,cr,id,_) => format("if"          ,e ,id,cr)
            case While     (e   ,cr,id,_) => format("while"       ,e ,id,cr)
            case Statement (stmt,cr,id,_) => format(stmt.toString ,"",id,cr)
            case Identifier(s   ,cr,id,_) => format("Identifier"  ,s ,id,cr)
            case Expression(e   ,cr,id,_) => format(e.toString,""    ,id,cr)
            case Switch    (e   ,cr,id,_) => format("switch"       ,e,id,cr)
            case For       (e   ,cr,id,_) => format("for"         ,if (e.isDefined) e.get else "no_cond",id,cr)
            case Empty     (_   ,   id,_) => format("Empty"       ,"",id,CodeRange(0,0,0,0))
        }
    }
}

// those classes will be the values of the CFG nodes
final case class If        (e: Expr             , cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
final case class For       (e: Option[Expr]     , cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
final case class While     (e: Expr             , cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
final case class Statement (stmt: SourceCodeNode, cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
final case class Identifier(s: String           , cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
final case class Expression(e: Expr             , cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
final case class Switch    (e: Expr             , cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)
// only used during the construction of the graph, should never be used in an actual CFG
private[ast] final case class Empty(              cr: CodeRange, _id: String, _isCopy: Boolean = false) extends ProgramNode(_id,_isCopy)

class CFGNode(value: ProgramNode) extends GraphNode[ProgramNode](value) {
    override def equals(that: Any) = that match { case x: CFGNode => value == x.value case _ => false }
    override def hashCode          = value.hashCode
}