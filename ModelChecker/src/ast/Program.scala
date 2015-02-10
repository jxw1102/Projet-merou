package ast

import ast.model.Decl
import ast.model.Expr
import ast.model.JumpStmt
import ast.model.Stmt
import cfg.Labelizable
import cfg.Labelizer
import ctl.Environment
import util.MutableMapView

/**
 * Those classes represent the most abstract and final form of the transformations of the source code
 * from AST just before the CFG conversion.
 * @author David Courtinot 
 * @author Sofia Boutahar
 * @author Xiaowen Ji      
 */
class Program {
    type MMV = MutableMapView[String,Decl]
    val declarations: MMV = MutableMapView()
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
 * Case-classes that will be the values of the CFG nodes. They implement the Visitor pattern against the ProgramNodeLabelizer
 * visitor class
 */
sealed abstract class ProgramNode(val id: String) extends Labelizable[ProgramNodeLabelizer] {
    type PNL = ProgramNodeLabelizer 
    
    override def equals(that: Any) = that match {
        case x: ProgramNode => id == x.id 
        case _              => false
    }
    override def hashCode = id.hashCode
    
    // ONLY for debugging purpose. I know it is very ugly...
    override def toString = {
        val format = (name: String, a: Any, id: String, cr: CodeRange) => "\"%s %s at %s %s\"".format(name,a,cr,id)
        this match {
            case If        (e   ,cr,id) => format("if"        ,e,id,cr)
            case For       (e   ,cr,id) => format("for"       ,if (e.isDefined) e.get else "no_cond",id,cr)
            case Empty     (_   ,   id) => format("Empty"     ,"",id,CodeRange(0,0,0,0))
            case While     (e   ,cr,id) => format("while"     ,e,id,cr)
            case Statement (stmt,cr,id) => format(stmt.toString ,"",id,cr)
            case Identifier(s   ,cr,id) => format("Identifier",s,id,cr)
            case Expression(e   ,cr,id) => format(e.toString,"",id,cr)
            case Switch    (e   ,cr,id) => format("switch"    ,e,id,cr)
        }
    }
}

final case class If        (e: Expr         , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitIf        (this) }
final case class For       (e: Option[Expr] , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitFor       (this) }
final case class While     (e: Expr         , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitWhile     (this) }
final case class Statement (stmt: Stmt      , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitStatement (this) }
final case class Identifier(s: String       , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitIdentifier(this) }
final case class Expression(e: Expr         , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitExpression(this) }
final case class Switch    (e: Expr         , cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitSwitch    (this) }
// only used during the construction of the graph, should never be used in an actual CFG
private[ast] final case class Empty(          cr: CodeRange, _id: String) extends ProgramNode(_id) { def visit(v: PNL) = v.visitEmpty     (this) }

trait ProgramNodeLabelizer extends Labelizer {
    def visitIf                (ifNode     : If        ): Option[Environment] = None
    def visitFor               (forNode    : For       ): Option[Environment] = None
    def visitWhile             (whileNode  : While     ): Option[Environment] = None
    def visitStatement         (stmt       : Statement ): Option[Environment] = None
    def visitIdentifier        (id         : Identifier): Option[Environment] = None
    def visitExpression        (expr       : Expression): Option[Environment] = None
    def visitSwitch            (switchNode : Switch    ): Option[Environment] = None
    private[ast] final def visitEmpty(empty: Empty     ): Option[Environment] = 
        throw new IllegalStateException("Empty nodes should never be explored")
}