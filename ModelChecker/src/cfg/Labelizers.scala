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
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 * @author David Courtinot
 */
protected abstract class CFGLabelizer extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] with ConvertEnv

case class IfLabelizer(pattern: ExprPattern) extends CFGLabelizer {
    override def test(t: ProgramNode) = t match {
        case If(expr,_,_) => pattern.matches(expr).toSet
        case _            => Set()
    }
}

case class ForLabelizer(pattern: Option[ExprPattern]) extends CFGLabelizer  {
    override def test(t: ProgramNode) = (t,pattern) match {
        case (For(Some(expr),_,_),Some(pattern)) => pattern.matches(expr).toSet
        case (For(None,_,_),None)                => Set(Top)
        case _                                   => Set() 
    }
}

case class WhileLabelizer(pattern: ExprPattern) extends CFGLabelizer  {
    override def test(t: ProgramNode) = t match {
        case While(expr,_,_) => pattern.matches(expr).toSet
        case _               => Set()
    }
}

case class SwitchLabelizer(pattern: ExprPattern) extends CFGLabelizer  {
    override def test(t: ProgramNode) = t match {
        case Switch(expr,_,_) => pattern.matches(expr).toSet
        case _                => Set()
    }
}

case class ExpressionLabelizer(pattern: ExprPattern) extends CFGLabelizer {
	override def test(t: ProgramNode) = t match {
		case Expression(e,_,_) => pattern.matches(e).toSet
		case _                 => Set() 
	}
}

case class FindExprLabelizer(pattern: ExprPattern) extends CFGLabelizer {
    private def foldRec (exprs: List[Expr])    = exprs.foldLeft(Set[Env]())((res,e) => res ++ recMatch(e))
	private def recMatch(expr: Expr): Set[Env] = pattern.matches(expr).toSet ++ foldRec(expr.getSubExprs)
	override def test(t: ProgramNode) = foldRec(ConvertNodes.getAllExprs(t)) 
}

case class MatchExprLabelizer(pattern: ExprPattern) extends CFGLabelizer {
    override def test(t: ProgramNode) = ConvertNodes.getExpr(t).map(pattern.matches(_)) match {
    	case Some(Some(res)) => Set(res)
        case _               => Set()
    }
}

class StatementLabelizer(val pattern: DeclPattern) extends CFGLabelizer {
    override def test(t: ProgramNode) = t match {
        case Statement(stmt: Decl,_,_) => pattern.matches(stmt).toSet
        case _ => Set() 
    }
}

case class VarDeclLabelizer(pattern: VarDeclPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => pattern.matches(decl).toSet
        case _                         => Set()
    }
}

case class VarDefLabelizer(pattern: VarDefPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => pattern.matches(decl).toSet
        case _                         => Set()
    }
}

//case class UnusedLabelizer(pattern: UndefinedVar) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
//    override def test(t: ProgramNode) = {
//        val x = ConvertNodes.getAllExprs(t).filter(_.isInstanceOf[DeclRefExpr]).toSet 
//        if(x.isEmpty) None 
//        else {
//        	val unused: Set[CFGVal] = x.map { case elt: DeclRefExpr => CFGDecl(elt.targetId, elt.typeOf, elt.targetName)}
//        	Some(new BindingsEnv -- (pattern.name -> unused))
//        }
//    }
//}