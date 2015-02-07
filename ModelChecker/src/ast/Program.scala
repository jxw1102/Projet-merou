package ast

import scala.collection.mutable.ArrayBuffer

import ast.model.Decl
import ast.model.Expr
import ast.model.JumpStmt
import ast.model.NullStmt
import cfg.GraphNode
import cfg.Labelizable
import cfg.Labelizer
import util.MutableMapView
import collection.mutable.{ Set => MSet }

/**
 * Those classes represent the most abstract and final form of the transformations of the source code
 * from AST just before the CFG conversion.
 * @author David Courtinot
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
    private[this] var _id       : Option[Long]      = None
    
    def codeRange                     = _codeRange
    def codeRange_=(range: CodeRange) = _codeRange = Some(range)
    
    def id             = _id
    def id_=(id: Long) = _id = Some(id)
    
    private val _next = MSet[SourceCodeNode]()
    private val _prev = MSet[SourceCodeNode]()
    
    def prev = _prev.toList
    def next = _next.toList
    
    def <<(v: SourceCodeNode): Unit = this match {
        case NullStmt() => 
        case _          => _prev += v; v._next += this
    }
    def <<(v: Option[SourceCodeNode]): Unit = v match {
        case Some(x) => this << x
        case None    => 
    }
    
    def >>(v: SourceCodeNode): SourceCodeNode = v match {
        case NullStmt() => v
        case _          => _next += v; v._prev += this; v 
    }
    def >>(v: Option[SourceCodeNode]): SourceCodeNode = v match {
        case Some(x) => this >> x
        case None    => this
    }
    
    override def toString = this.getClass.getSimpleName + "_" + id.get.toHexString
    def mkString = { set.clear(); addString(new StringBuilder).toString }
    val set = MSet[SourceCodeNode]()
    private def addString(sb: StringBuilder): StringBuilder = {
        _next.foreach(node => sb.append("%s -> %s;\n".format(this,node)))
        if (set contains this) sb 
        else {
        	set += this
            _next/*.filterNot(set contains _)*/.foreach(_.addString(sb))
            sb
        }
    }
}

object SourceCodeNode {
    def apply(node: SourceCodeNode, codeRange: CodeRange, id: Long) = { node.codeRange = codeRange; node.id = id; node }
}

/**
 * Case-classes that will be the values of the CFG nodes. They implement the Visitor pattern against the ProgramNodeLabelizer
 * visitor class
 */
sealed abstract class ProgramNode extends Labelizable[ProgramNodeLabelizer] { type PNL = ProgramNodeLabelizer }
final case class If        (expr: Expr, range: CodeRange, id: Long)              extends ProgramNode { def visit(v: PNL) = v.visitIf        (this) }
final case class For       (expr: Expr, range: CodeRange, id: Long)              extends ProgramNode { def visit(v: PNL) = v.visitFor       (this) }
final case class While     (expr: Expr, range: CodeRange, id: Long)              extends ProgramNode { def visit(v: PNL) = v.visitWhile     (this) }
final case class Identifier(s: String , range: CodeRange, id: Long)              extends ProgramNode { def visit(v: PNL) = v.visitIdentifier(this) }
final case class Expression(expr: Expr, range: CodeRange, id: Long)              extends ProgramNode { def visit(v: PNL) = v.visitExpression(this) }
final case class Assignment(left: Expr, right: Expr, range: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitAssignment(this) }

// special definition to use only for the conversion to CFG
private[ast] case class Jump(jump: JumpStmt, range: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = throw new UnsupportedOperationException }

trait ProgramNodeLabelizer extends Labelizer {
    def visitIf        (ifNode   : If        )
    def visitFor       (forNode  : For       )
    def visitWhile     (whileNode: While     )
    def visitAssignment(stmt     : Assignment)
    def visitIdentifier(id       : Identifier)
    def visitExpression(expr     : Expression)
}

///////////////////////////////////////////////////////
//              Example of a labelizer
///////////////////////////////////////////////////////
//case class BinaryOperator(symbol: String)
//
//class ExprLabel extends ProgramNodeLabelizer {
//    val left: Metavar = DefinedMetavar(Variable("x"))
//    val right: Metavar = UndefinedMetavar("X")
//    val op = BinaryOperator("==")
//    
//    def visitIf        (ifNode   : If        ) = None
//    def visitFor       (forNode  : For       ) = ???
//    def visitWhile     (whileNode: While     ) = ???
//    def visitAssignment(stmt     : Assignment) = ???
//    def visitIdentifier(id       : Identifier) = ???
//    def visitExpression(expr     : Expression) = ???
//}
//
//abstract class Metavar 
//case class DefinedMetavar(value: Expr) extends Metavar
//case class UndefinedMetavar(name: String) extends Metavar
