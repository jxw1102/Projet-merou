package cfg

import scala.reflect.runtime.universe
import ast.Expression
import ast.For
import ast.If
import ast.ProgramNode
import ast.Statement
import ast.While
import ast.model._
import ctl.BindingsEnv
import ctl.Bottom
import ctl.ConvertEnv
import ctl.Environment
import ctl.Labelizer
import ctl.MetaVariable
import ctl.TypeOf
import ctl.Value
import ctl.PosBinding
import ast.Switch
import ctl.Top

/**
 * This file contains all the Labelizer(s) we defined on a CFG. They allow for the evaluation of some base 
 * predicates that can be composed to test more complicated properties.
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 * @author David Courtinot
 */
protected abstract class CFGLabelizer extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] with ConvertEnv

/**
 * Labelizes every If node matching a given expression pattern
 */
case class IfLabelizer(pattern: ExprPattern) extends CFGLabelizer {
    override def test(t: ProgramNode) = t match {
        case If(expr,_,_) => pattern.matches(expr).toSet
        case _            => Set()
    }
}

/**
 * Labelizes every For node matching a given expression pattern. If the pattern is None, it will only match
 * a For node with an empty condition.
 */
case class ForLabelizer(pattern: Option[ExprPattern]) extends CFGLabelizer  {
    override def test(t: ProgramNode) = (t,pattern) match {
        case (For(Some(expr),_,_),Some(pattern)) => pattern.matches(expr).toSet
        case (For(None,_,_),None)                => Set(Top)
        case _                                   => Set() 
    }
}

/**
 * Labelizes every While node matching a given expression pattern
 */
case class WhileLabelizer(pattern: ExprPattern) extends CFGLabelizer  {
    override def test(t: ProgramNode) = t match {
        case While(expr,_,_) => pattern.matches(expr).toSet
        case _               => Set()
    }
}

/**
 * Labelizes every Switch node matching a given expression pattern
 */
case class SwitchLabelizer(pattern: ExprPattern) extends CFGLabelizer  {
    override def test(t: ProgramNode) = t match {
        case Switch(expr,_,_) => pattern.matches(expr).toSet
        case _                => Set()
    }
}

/**
 * Labelizes every Expression node matching a given expression pattern
 */
case class ExpressionLabelizer(pattern: ExprPattern) extends CFGLabelizer {
	override def test(t: ProgramNode) = t match {
		case Expression(e,_,_) => pattern.matches(e).toSet
		case _                 => Set() 
	}
}

/**
 * Labelizes every node wich contains at least one occurrence of a given expression pattern.
 * It will return all the occurrences of the pattern in this node.
 */
case class FindExprLabelizer(pattern: ExprPattern) extends CFGLabelizer {
    private def foldRec (exprs: List[Expr])    = exprs.foldLeft(Set[Env]())((res,e) => res ++ recMatch(e))
	private def recMatch(expr: Expr): Set[Env] = pattern.matches(expr).toSet ++ foldRec(expr.getSubExprs)
	override def test   (t: ProgramNode)       = foldRec(ConvertNodes.getExpr(t).toList)
}

/**
 * Labelizes every node wich produces an expression matching exactly a given expression pattern. 
 * Contrarily to the FindExprLabelizer, this will return a single result if the expression matched
 * the pattern.
 */
case class MatchExprLabelizer(pattern: ExprPattern) extends CFGLabelizer {
    override def test(t: ProgramNode) = ConvertNodes.getExpr(t).map(pattern.matches(_)) match {
    	case Some(Some(res)) => Set(res)
        case _               => Set()
    }
}

/**
 * Labelizes every Statement node containing a VarDecl matching a given declaration pattern.
 * @note Returns an environement containing a CFGDecl. Ask yourself which semantic you need
 *       between VarDeclLabelizer and VarDefLabelizer
 */
case class VarDeclLabelizer(pattern: VarDeclPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => pattern.matches(decl).toSet
        case _                         => Set()
    }
}



//case class FunctionDeclLabelizer(pattern: FunctionDeclPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
//    override def test(t: ProgramNode) = t match {
//        case Statement(decl: Decl,_,_) => pattern.matches(decl).toSet
//        case _                         => Set()
//    }
//}


/**
 * Labelizes every Statement node containing a VarDecl matching a given declaration pattern.
 * @note Returns an environement containing a CFGDef. Ask yourself which semantic you need
 *       between VarDefLabelizer and VarDeclLabelizer
 */
case class VarDefLabelizer(pattern: VarDefPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => pattern.matches(decl).toSet
        case _                         => Set()
    }
}

case class UseLabelizer(pattern: StringPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    private def foldRec (exprs: List[Expr])    = exprs.foldLeft(Set[Env]())((res,e) => res ++ recMatch(e))
    private def recMatch(expr: Expr): Set[Env] = expr match {
        case DeclRefExpr(id,targetName,targetId) => pattern.matches(targetName) match {
            case Some(BindingsEnv(bind)) => println(expr + " : : " + expr.getClass)
                val stringToCFGDecl = bind.mapValues(x => CFGDecl(targetId, expr.getType, targetName))
                Set((new BindingsEnv ++ (stringToCFGDecl.toSeq: _*)))
            case _                       => Set()
        }
        case _                           => foldRec(expr.getSubExprs)
    }
    override def test   (t: ProgramNode)       = t match {
        case Statement(VarDecl(name,typeOf,expr),_,_) => foldRec(expr.toList)
        case _                                        => foldRec(ConvertNodes.getExpr(t).toList)
    }
}