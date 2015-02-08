package ast

import scala.collection.mutable.{Set => MSet}
import scala.util.Random

import ast.model.Decl
import ast.model.Expr
import ast.model.Stmt
import ast.util.MutableMapView
import cfg.Labelizable
import cfg.Labelizer
import util.MutableMapView

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
    
    def prev = _prev
    def next = _next
    
    def <<(v: SourceCodeNode): Unit = this match {
        case _ if !this.id.isDefined => 
        case _                       => _prev += v; v._next += this
    }
    def <<(v: Option[SourceCodeNode]): Unit = v match {
        case Some(x) => this << x
        case None    => 
    }
    
    def >>(v: SourceCodeNode): SourceCodeNode = v match {
        case _ if !v.id.isDefined => v
        case _                    => _next += v; v._prev += this; v 
    }
    def >>(v: Option[SourceCodeNode]): SourceCodeNode = v match {
        case Some(x) => this >> x
        case None    => this
    }
    
	override def hashCode() = id match {
        case None    => Random.nextInt()
        case Some(x) => x.toInt
    }
    
	override def equals(obj: Any) = obj match {
        case SourceCodeNode => obj.asInstanceOf[SourceCodeNode].id == this.id
        case _              => false
    }
    
    private def notNone(n: Option[Long]) = if (n.isDefined) n.get else 0 
	override def toString = this.getClass.getSimpleName + "_" + notNone(id).toHexString
    
    def mkString = addString(new StringBuilder, MSet()).toString
	private def addString(sb: StringBuilder, set: MSet[SourceCodeNode]): StringBuilder = {
        _next.foreach(node => sb.append("%s -> %s;\n".format(this,node)))
        if (set contains this) sb 
        else {
        	set += this
            _next.filterNot(set contains _).foreach(_.addString(sb,set))
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
sealed abstract class ProgramNode extends Labelizable[ProgramNodeLabelizer] {
    type PNL = ProgramNodeLabelizer 
    // ONLY for debugging purpose. I know it is very ugly...
    override def toString = {
        val format = (name: String, id: Long) => "%s_(0x%s)".format(name,java.lang.Long.toHexString(id))
        this match {
            case If        (_,_,id) => format("If"        ,id)
            case For       (e,_,id) => format("For"       ,if (e.isDefined) e.get.id.get else id)
            case Empty     (_,  id) => format("Empty"     ,id)
            case While     (e,_,id) => format("While"     ,e.id.get)
            case Statement (_,_,id) => format("Statement" ,id)
            case Identifier(_,_,id) => format("Identifier",id)
            case Expression(_,_,id) => format("Expression",id)
        }
    }
}

final case class If        (e: Expr         , cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitIf        (this) }
final case class For       (e: Option[Expr] , cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitFor       (this) }
final case class Empty     (                  cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitEmpty     (this) }
final case class While     (e: Expr         , cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitWhile     (this) }
final case class Statement (stmt: Stmt      , cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitStatement (this) }
final case class Identifier(s: String       , cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitIdentifier(this) }
final case class Expression(e: Expr         , cr: CodeRange, id: Long) extends ProgramNode { def visit(v: PNL) = v.visitExpression(this) }

trait ProgramNodeLabelizer extends Labelizer {
    def visitIf        (ifNode   : If        )
    def visitFor       (forNode  : For       )
    def visitEmpty     (empty    : Empty     )
    def visitWhile     (whileNode: While     )
    def visitStatement (stmt     : Statement )
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
