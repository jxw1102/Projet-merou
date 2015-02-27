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

/**
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 * @author David Courtinot
 */

case class IfLabelizer(pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode) = t match {
        case If(expr,_,_) => pattern.matches(expr).toSet
        case _            => Set()
    }
}

case class ForLabelizer(pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal]  {
    override def test(t: ProgramNode) = t match {
        case For(Some(expr),_,_) => pattern.matches(expr).toSet
        case _                   => Set() 
    }
}

case class WhileLabelizer(pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal]  {
    override def test(t: ProgramNode) = t match {
        case While(expr,_,_) => pattern.matches(expr).toSet
        case _               => Set()
    }
}

case class ExpressionLabelizer(pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
	override def test(t: ProgramNode) = t match {
		case Expression(e,_,_) => pattern.matches(e).toSet
		case _                 => Set() 
	}
}

case class FindExprLabelizer(pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
	private def foldRec(exprs: List[Expr])     = exprs.foldLeft[Set[Env]](Set())((res,e) => res ++ recMatch(e))
	private def recMatch(expr: Expr): Set[Env] = pattern.matches(expr).toSet ++ foldRec(expr.getSubExprs)
	override def test(t: ProgramNode)          = foldRec(ConvertNodes.getAllExprs(t))
}


//case class ArithmeticPointerLabelizer() extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
//    override def test(t: ProgramNode) = {
//    	ConvertNodes.getAllExprs(t).find {
//    	    case x: CompoundAssignOp if x.isPointer => true
//    	    case x: BinaryOp         if x.isPointer => true
//    	    case x: UnaryOp          if x.isPointer => x.operator != "++" && x.operator != "--"
//    	    case _                                  => false
//    	} match {
//    	    case Some(_) => Some(new BindingsEnv)
//    	    case None    => None
//    	}
//    }
//}
        
class StatementLabelizer(val pattern: DeclPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(stmt: Decl,_,_) => pattern.matches(stmt).toSet
        case _ => Set() 
    }
}

//case class InfeasiblePathLabelizer() extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
//	import InfeasiblePathLabelizer._
//	
//	private def checkPattern(expr: Expr): Option[Env] = IDENTITY.matches(expr)
//		.orElse(LITERAL_ASSIGN(expr)) 
//		.orElse(LITERAL_EXPR(expr)) match {
//			case Some(_) => Some(new BindingsEnv)
//			case None    => None
//		}
//
//	override def test(t: ProgramNode) = t match {
//		case If(Literal(_,_),_,_) | While(Literal(_,_),_,_) | For(None|Some(Literal(_,_)),_,_) |
//			Switch (Literal(_,_),_,_) => Some(new BindingsEnv) 
//		case If    (expr      ,_,_)   => checkPattern(expr) 
//		case While (expr      ,_,_)   => checkPattern(expr) 
//		case For   (Some(expr),_,_)   => checkPattern(expr) 
//		case Switch(expr      ,_,_)   => checkPattern(expr) 
//		case _                        => None
//	}
//}
//
//object InfeasiblePathLabelizer {
//    private val IDENTITY       = BinaryOpPattern(UndefinedVar("X"),UndefinedVar("X"),"==")
//    private val LITERAL_EXPR   = (expr: Expr) => if (isAllLiteral(expr)) Some(new BindingsEnv) else None
//    private val LITERAL_ASSIGN = (expr: Expr) => {
//        val pattern = BinaryOpPattern(UndefinedVar("X"),UndefinedVar("Y"),"=")
//        pattern.matches(expr) match {
//            case Some(env) => env("Y") match {
//                case CFGExpr(e) if (isAllLiteral(e)) => Some(new BindingsEnv)
//                case _                                       => None
//            }
//            case _  => None
//        }
//    }
//    
//    private def isAllLiteral(expr: Expr): Boolean = expr match {
//        case Literal(_, _) => true
//        case _             => 
//            val exprs = expr.getSubExprs
//            if (exprs.isEmpty) false else exprs.forall(isAllLiteral(_))
//    }
//}

case class VarDeclLabelizer(pattern: VarDeclPattern) extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Statement(decl: Decl,_,_) => pattern.matches(decl).toSet
        case _                         => Set()
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
                    Set((new BindingsEnv).++(values: _*))
                case _                    => Set()
            }
        case _ => Set()
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

//case class UnusedFunctionValue() extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
//	private def foldRec(exprs: List[Expr]): Option[Env] = exprs.foldLeft[Option[Env]](None)((res,e) => if (res.isDefined) res else recMatch(e))
//    private def recMatch(expr: Expr): Option[Env] = { 
//        val res: Option[Env] = expr match {
//        	case CallExpr(typeOf,_) if typeOf != "void" => Some(new BindingsEnv)
//        	case _                                      => None
//        } 
//        res orElse foldRec(expr.getSubExprs)
//    }
//    
//    override def test(t: ProgramNode) = t match {
//        case Expression(BinaryOp(_,_,_,"="),_,_) | Expression(CompoundAssignOp(_,_,_,_),_,_) => None
//        case Expression(expr,_,_) => recMatch(expr)
//        case _                    => None
//    }
//}