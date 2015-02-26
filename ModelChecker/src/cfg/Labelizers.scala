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

/**
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 * @author David Courtinot
 */

// TODO
// unused variables : Decl(X) and AX(Unused(X))
// hidden definitions : Decl(X) and EF(Decl(Y) and SameDef(X,Y))

/* 
 * /////////////////////// Labelizers ///////////////////////
 */

class IfLabelizer(val pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode) = t match {
        case If(expr,_,_) => pattern.matches(expr) 
        case _            => None 
    }
}

class ForLabelizer(val pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal]  {
    override def test(t: ProgramNode) = t match {
        case For(Some(expr),_,_) => pattern.matches(expr) 
        case _                   => None 
    }
}

class WhileLabelizer(val pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal]  {
    override def test(t: ProgramNode) = t match {
        case While(expr,_,_) => pattern.matches(expr)
        case _               => None 
    }
}

class ExpressionLabelizer(val pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
	override def test(t: ProgramNode) = t match {
		case Expression(e,_,_) => pattern.matches(e)
		case _                 => None 
	}
}

case class FindExprLabelizer(pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
	private def foldRec(exprs: List[Expr])        = exprs.foldLeft[Option[Env]](None)((res,e) => if (res.isDefined) res else recMatch(e))
	private def recMatch(expr: Expr): Option[Env] = pattern.matches(expr).orElse(foldRec(expr.getSubExprs))
	override def test(t: ProgramNode)             = foldRec(ConvertNodes.getExprs(t))
}

case class ArithmeticPointerLabelizer() extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode) = 
    	ConvertNodes.getExprs(t).find {
    	    case x: CompoundAssignOp if x.isPointer => true
    	    case x: BinaryOp         if x.isPointer => true
    	    case x: UnaryOp          if x.isPointer => true
    	    case _                                  => false
    	} match {
    	    case Some(_) => Some(new BindingsEnv)
    	    case None    => None
    	}
}
        
class StatementLabelizer(val pattern: DeclPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode): Option[Environment[CFGMetaVar, CFGVal]] = t match {
        case Statement(stmt: Decl,_,_) => pattern.matches(stmt)
        case _ => None     
    }
}

case class DeadIfLabelizer() extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    
    def isAllLiteral(expr: Expr): Boolean = expr match {
        case Literal (_, _) => true
        case _              => 
            val tempList = expr.getSubExprs
            tempList.map(isAllLiteral(_)).foldLeft(!tempList.isEmpty)((a,b) => a && b)
        }
        
    override def test(t: ProgramNode) = t match {
        case If(BinaryOp   (_,_,r,"="),_,_) if (isAllLiteral(r))    => Some(new BindingsEnv) 
        case If(expr,_,_)                   if (isAllLiteral(expr)) => Some(new BindingsEnv)
        case _                                                        => None
    }
}

case class ReturnLabelizer(pattern: ExprPattern)  extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(ReturnStmt(_,expr),_,_) => Some(new BindingsEnv ++ (CFGMetaVar("X") -> CFGExpr(expr)))
        case _                                 => None
    }
}

case class VarDeclLabelizer(pattern: VarDeclPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => pattern.matches(decl)
        case _                         => None
    }
}

case class VarDefLabelizer(pattern: VarDeclPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => 
            val env: Option[Env] = pattern.matches(decl)
            env match {
                case Some(BindingsEnv(b)) => 
                    val values: Seq[(CFGMetaVar,CFGVal)] = 
                        b.mapValues { case PosBinding(CFGDecl(_,x,y)) => CFGDef(x,y) }.toSeq
                    Some((new BindingsEnv).++(values: _*))
                case _                    => None
            }
        case _ => None
    }
}

case class UnusedLabelizer(pattern: UndefinedVar) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
//    private def foldRec(exprs: List[Expr])        = exprs.foldLeft[Option[Env]](None)((res,e) => if (res.isDefined) res else recMatch(e))
//    private def recMatch(expr: Expr): Option[Env] = pattern.matches(expr).orElse(foldRec(expr.getSubExprs))

    override def test(t: ProgramNode) = {
        val x = ConvertNodes.getExprs(t).filter(_.isInstanceOf[DeclRefExpr]).toSet 
        
        if(x.isEmpty)
            None 
        else{
        	val unusedVar:Set[CFGVal] = x.map{ case elt: DeclRefExpr => CFGDecl(elt.targetId, elt.typeOf, elt.targetName)}
        	Some(new BindingsEnv -- (pattern.name -> unusedVar))
        }
    }
}