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

/**
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 */

/* 
 * /////////////////////// Generic type definitions : Value and MetaVariable ///////////////////////
 */
case class CFGMetaVar(name: String) extends MetaVariable {
    override def hashCode       = name.hashCode
    override def toString       = name

    override def equals(a: Any) = a match {
        case CFGMetaVar(value) => value == name
        case _                 => false 
    }
}

sealed abstract class CFGVal extends TypeOf[CFGVal] with Value
final case class CFGExpr(expr: Expr) extends CFGVal {
    override def toString        = expr.toString
    override def cast(n: CFGVal) = n match { case CFGExpr(_) => true; case _ => false }    
}
final case class CFGDecl (ident: DeclRefExpr) extends CFGVal {
    override def cast(n: CFGVal) = n match { case CFGDecl(_) => true; case _ => false }
}

/* 
 * /////////////////////// Pattern definition ///////////////////////
 */
sealed abstract class Pattern extends ConvertEnv {
    type Env = Environment[CFGMetaVar,CFGVal]

    def matchEnv(expr: Expr): Env = this match {
        case DefinedExpr  (e   : Expr  )     => if (e matches expr) new BindingsEnv else Bottom
        case UndefinedVar (name: CFGMetaVar) => new BindingsEnv ++ (name -> expr)
        case _                               => Bottom
    }
    
    def matchDeclName(targetName: String, targetType: String, targetId: String): Env = this match {
        case DefinedDecl  (s   : String)     => if (s == targetName) new BindingsEnv else Bottom
        ////////////////////////////////////////////////////////////////////////////////////////////////
        // Cette ligne est assez bizarre. "Var" n'a aucun sens dans l'AST, à quoi sert cette fonction ?
        ////////////////////////////////////////////////////////////////////////////////////////////////
        case UndefinedVar (name: CFGMetaVar) => new BindingsEnv ++ (name -> CFGDecl(DeclRefExpr("",targetType,targetName,targetId,"Var"))) 
        ////////////////////////////////////////////////////////////////////////////////////////////////
        case _                               => Bottom
    }
}

case class UndefinedVar (name: CFGMetaVar) extends Pattern
case class DefinedExpr  (expr: Expr      ) extends Pattern
case class DefinedDecl  (name: String    ) extends Pattern

trait ExprPattern extends Pattern with ConvertEnv {
    def matches (expr: Expr): Option[Env] = expr match {
    	case CallExpr(rtn,paramsFun)          => matchesCallExpr(rtn, paramsFun)
        case CompoundAssignOp (_,l,r,_)       => matchesCompoundAssignOp(l,r)
        case BinaryOp         (_,l,r,op)      => matchesBinaryOp(l,r,op)
        case UnaryOp          (_,x,op,k)      => matchesUnaryOp(x,op,k)
        case ConditionalOperator(_,(x,y,z),_) => matchesConditionalOperator(x,y,z)
        case _                                => None
    }

    def matchesCallExpr(rtn: String, paramsFun: List[Expr]) : Option[Env] = None
    def matchesCompoundAssignOp (l: Expr, r: Expr) : Option[Env] = (this.matches(l),this.matches(r)) match {
    	case (Some(bind1), Some(bind2)) => 
    	val temp = bind1 & bind2
    	temp match {
    		case BindingsEnv(_) => Some(temp)
    		case _              => None
    	}
    	case (Some(bind), _) => Some(bind)
    	case (_, Some(bind)) => Some(bind)
    	case _               => None 
    }
    def matchesBinaryOp (l: Expr, r: Expr, op: String) : Option[Env] = (this.matches(l),this.matches(r)) match {
    	case (Some(bind1), Some(bind2)) => 
    	val temp = bind1 & bind2
    	temp match {
    		case BindingsEnv(_) => Some(temp)
    		case _              => None
    	}
    	case (Some(bind), _) => Some(bind)
    	case (_, Some(bind)) => Some(bind)
    	case _               => None 
    }
    def matchesConditionalOperator(x: Expr, y: Expr,z: Expr) : Option[Env] = (this.matches(x),this.matches(y),this.matches(z)) match {
    	case (Some(bind1), Some(bind2), Some(bind3)) => 
    	val temp = bind1 & bind2 & bind3
    	temp match {
    		case BindingsEnv(_) => Some(temp)
    		case _              => None
    	}
    	case (Some(bind), _, _) => Some(bind)
    	case (_, Some(bind), _) => Some(bind)
    	case (_, _, Some(bind)) => Some(bind)
    	case _                  => None 
    }
    def matchesUnaryOp (operand: Expr, operator: String, kind: OpPosition): Option[Env] = this.matches(operand) 
}

case object Anything extends ExprPattern {
    override def matches(expr: Expr): Option[Env] = Some(new BindingsEnv)
}

// This class works for just an expression 
case class OnePattern (op: Pattern) extends ExprPattern {   
    override def matches(expr: Expr): Option[Env] = {
        val mat = op.matchEnv(expr) 
        mat match {
            case BindingsEnv(_) => Some(mat)
            case _              => None
        }
    }
}

// This class works for BinaryOp and CompoundAssignOp
case class BinaryOpPattern (left: Pattern, right: Pattern, op: String) extends ExprPattern { 

    override def matchesBinaryOp (l: Expr, r: Expr, operator: String): Option[Env] = {
        if (operator == op) {
            val inter = left.matchEnv(l) & right.matchEnv(r)
            inter match {
                case BindingsEnv(_) => Some(inter)
                case _              => None
            }    
        }
        else None
    }
}

case class UnaryOpPattern (operand: Pattern, op: String, kind: OpPosition) extends ExprPattern {
    override def matchesUnaryOp (operand: Expr, operator: String, kind: OpPosition): Option[Env] = 
        if (operator == op && this.kind == kind) {
            val env = this.operand.matchEnv(operand)
            env match {
                case BindingsEnv(_) => Some(env)
                case _                 => None
            }
        }
        else None        
}

case class CallExprPattern(params: List[Pattern], rtnType: Option[String] = None) extends ExprPattern {
    def matchesParams (paramsFun : List[Expr]): Option[Env] = {
        var binding: Env = new BindingsEnv[CFGMetaVar, CFGVal]
        
        for(tup <- this.params.zip(paramsFun)) tup match {
            case (pe,e) => binding = binding & pe.matchEnv(e)  
                binding match {
                    case BindingsEnv(_) =>  
                    case _              =>  return None
                }
            case _ => return None
        }
        Some(binding)
    }
    
    override def matchesCallExpr (rtn: String, paramsFun: List[Expr]) : Option[Env] = {
        if (paramsFun.size == params.size) {
                  rtnType match {
                      case Some(value) => if (value == rtn) matchesParams(paramsFun) else None
                      case _           => matchesParams(paramsFun)
                  } 
              }
        else None
    }
}

trait DeclPattern extends Pattern with ConvertEnv {
	def matches(decl: Decl): Option[Env]
}

case class VarDeclPattern(varName: Pattern, typeNameDecl: Option[String], valueDecl: Option[Pattern] = None) extends DeclPattern {
    override def matches(decl: Decl): Option[Env] = {
        decl match {   
            case VarDecl(name, typeName, value)  => 
                var typeDecl: String = ""
                typeNameDecl match {
                    case Some(value) => if(typeName != value) return None else typeDecl = value 
                    case _           => 
                }
                
                val nameEnv = varName.matchDeclName(name, typeDecl, decl.id.get)
                val resEnv: Option[Env] = (value,valueDecl) match {
                    case (Some(expr), Some(exprDecl)) => Some(nameEnv & exprDecl.matchEnv(expr))  
                    case (None, None)                 => Some(nameEnv & new BindingsEnv)    
                    case (_, _)                       => None
                }
                    
                resEnv match {
                    case Some(BindingsEnv(bind)) => resEnv
                    case _                       => None
                }
            case _  => None
        }
    }
}


/* 
 * /////////////////////// Labilizers ///////////////////////
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

case class ArithmeticPointerLabelizer() extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode) = 
    	ProgramNode.convert(t).filter(_.isInstanceOf[CFGExpr]).map { case CFGExpr(expr) => expr }.find {
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

class UseLabelizer(val pattern: ExprPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode) = t match {
        case Expression(e,_,_)                      => pattern.matches(e)
        case While     (e,_,_)                      => pattern.matches(e)
        case If        (e,_,_)                      => pattern.matches(e) 
        case For       (Some(e),_,_)                => pattern.matches(e) 
        case Statement (VarDecl(_, _, Some(e)),_,_) => pattern.matches(e)
        case _                                      => None 
    }
} 

case class DeadIfLabelizer() extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode): Option[Environment[CFGMetaVar, CFGVal]] = t match {
        case If(Literal(_,_,_),_,_) => Some(new BindingsEnv) 
        case _                      => None
    }
}

case class ReturnLabelizer(pattern: ExprPattern)  extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
    override def test(t: ProgramNode): Option[Environment[CFGMetaVar, CFGVal]] = t match {
        case Statement(ReturnStmt(_,expr),_,_) => Some(new BindingsEnv ++ (CFGMetaVar("X") -> CFGExpr(expr)))
        case _                                 => None
    }
}
//
//case class DeclLabelizer(pattern: DeclPattern)  extends Labelizer[CFGMetaVar,ProgramNode,CFGVal] {
//    override def test(t: ProgramNode): Option[Environment[CFGMetaVar, CFGVal]] = t match {
//        case Decl() => 
//        case _                                 => None
//    }
//}
