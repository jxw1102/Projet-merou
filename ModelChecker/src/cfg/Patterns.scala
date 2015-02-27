package cfg

import ctl.ConvertEnv
import ast.model.OpPosition
import ctl.BindingsEnv
import ast.model.Expr
import ast.model.DeclRefExpr
import ctl.Bottom
import ctl.Environment
import ast.model.VarDecl
import ast.model.CallExpr
import ast.model.BinaryOp
import ast.model.UnaryOp
import ast.model.Decl
import ast.model.CompoundAssignOp
import ast.model.ParamVarDecl

////////////////////////////////////////////////////////////////////////////////
//                                BASE PATTERNS
////////////////////////////////////////////////////////////////////////////////
sealed trait Pattern extends ConvertEnv {
    type Env = Environment[CFGMetaVar,CFGVal]
}

trait DeclPattern extends Pattern {
	def matches(decl: Decl): Option[Env]
}

trait StringPattern extends Pattern {
	def matches(s: String): Option[Env]
}

trait ExprPattern extends Pattern {
    def matches (expr: Expr): Option[Env] 
}

object ExprPattern {
    type Env = Environment[CFGMetaVar,CFGVal]
    
    def intersection(e0: Option[Env], e1: Option[Env]) = (e0,e1) match {
        case (Some(x),Some(y)) => val inter = x & y; inter match { case BindingsEnv(_) => Some(inter) case _ => None }
        case _                 => None
    }
}

// Right now, we are not willing to make ExprPattern a recursive structure
// The aim of this trait is to avoid that by allowing only them to be composed
// to make composed patterns (one level of recursivity)
// 
trait AtomicExprPattern extends ExprPattern

case class UndefinedVar (name: CFGMetaVar) extends AtomicExprPattern with DeclPattern with StringPattern {
    override def matches(expr: Expr  ) = Some(new BindingsEnv ++ (name -> expr))
    override def matches(decl: Decl  ) = Some(new BindingsEnv ++ (name -> decl))
    override def matches(s   : String) = Some(new BindingsEnv ++ (name -> s))
}
case class DefinedExpr  (expr: Expr) extends AtomicExprPattern {
    override def matches(e: Expr) = if (e matches expr) Some(new BindingsEnv) else None
}
case class DefinedString(name: String) extends StringPattern {
    override def matches(s: String) = if (s == name) Some(new BindingsEnv) else None
}
case class NotString    (not: Set[String]=Set()) extends StringPattern {
    override def matches(s: String) = if (not contains s) None else Some(new BindingsEnv)
}

////////////////////////////////////////////////////////////////////////////////
//                                ADVANCED PATTERNS
////////////////////////////////////////////////////////////////////////////////
case class BinaryOpPattern (left: AtomicExprPattern, right: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern { 
    import ExprPattern._
    override def matches(expr: Expr) = expr match {
    	case BinaryOp(_,l,r,operator) => intersection(intersection(op.matches(operator),left.matches(l)),right.matches(r))
        case _                        => None
   }
}

case class CompoundAssignOpPattern (left: AtomicExprPattern, right: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern { 
	import ExprPattern._
	override def matches (expr: Expr) = expr match {
		case CompoundAssignOp(_,l,r,operator) => intersection(intersection(op.matches(operator),left.matches(l)),right.matches(r))
		case _                                => None
	}
}

case class AssignmentPattern(left: AtomicExprPattern, right: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern {
    import ExprPattern._
    private val patterns: List[ExprPattern] = op match {
        case DefinedString("=")                       => List(BinaryOpPattern        (left,right,op))
        case DefinedString("+=" | "-=" | "*=" | "/=") => List(CompoundAssignOpPattern(left,right,op))
        case DefinedString(_)                         => throw new IllegalArgumentException(op + " is not an assignment operator")
        case _                                        => List(BinaryOpPattern        (left,right,op),
                											  CompoundAssignOpPattern(left,right,op))
    }
    override def matches(expr: Expr): Option[Environment[CFGMetaVar,CFGVal]] = patterns.map(_.matches(expr)).reduce(intersection(_,_))
}


case class UnaryOpPattern (operand: AtomicExprPattern, op: String, kind: OpPosition) extends ExprPattern {
    override def matches (expr: Expr) = expr match {
    	case UnaryOp (_,operand, operator, kind) if (operator == op && this.kind == kind) => this.operand.matches(operand)
        case _                                                                            => None
   }
}

case class CallExprPattern(
        name : StringPattern, 
        params: Option[List[AtomicExprPattern]]=None,
        typeOf: StringPattern=NotString(Set())) extends ExprPattern {
    
    def matchesParams (paramsFun : List[Expr]): Option[Env] = params match {
        case None        => Some(new BindingsEnv)
        case Some(value) =>
            if (paramsFun.size != value.size) None 
            else value.zip(paramsFun).foldLeft[Option[Env]](Some(new BindingsEnv)) { 
                case (acc,(pe,e)) => if (acc.isEmpty) None else ExprPattern.intersection(acc,pe.matches(e)) 
            }
    }
    
    private def matchFun(ref: DeclRefExpr): Option[Env] = (name.matches(ref.targetName),name) match {
        case (Some(_),UndefinedVar(x))  => Some(new BindingsEnv ++ (x -> CFGExpr(ref)))
        case (Some(_),DefinedString(_)) => Some(new BindingsEnv)
        case _                          => None
    }
    
    override def matches(expr: Expr) = expr match {
    	case CallExpr(rtn,ref,paramsFun) => typeOf.matches(rtn) match {
            case Some(_) => ExprPattern.intersection(matchFun(ref),matchesParams(paramsFun)) 
            case _       => None
        } 
        case _                           => None
   }
}

case class VarDeclPattern(typeOf: Option[String], name: StringPattern) extends DeclPattern {
    private def matchDecl(decl: Decl): Option[Env] = (name.matches(decl.name),name) match {
        case (Some(_),UndefinedVar(x))  => Some(new BindingsEnv ++ (x -> CFGDecl(decl.id.get,decl.typeOf,decl.name)))
        case (Some(_),DefinedString(_)) => Some(new BindingsEnv)
        case _                          => None
    }
    
    override def matches(decl: Decl): Option[Env] = decl match {
        case VarDecl(declName,typeName,_) =>
            (typeOf) match {
                case Some(value) if(value == typeName) => matchDecl(decl)
                case None                              => matchDecl(decl)
                case _                                 => None 
            }
        case _ => None
    }
}

//case class ParamVarDeclPattern extends DeclPattern {
//    
//}

//case class FunctionDeclPattern(name: StringPattern, args: Option[List[ParamVarDeclPattern]]) extends DeclPattern {
//    private def matchDecl(decl: Decl): Option[Env] = (name.matches(decl.name),name) match {
//        case (Some(_),UndefinedVar(x))  => Some(new BindingsEnv ++ (x -> CFGDecl(decl.id.get,decl.typeOf,decl.name)))
//        case (Some(_),DefinedString(_)) => Some(new BindingsEnv)
//        case (None,_)    => None
//    }
//}



//case class VarDeclPattern(varName: Pattern, typeNameDecl: Option[String], valueDecl: Option[Pattern] = None) extends DeclPattern {
//    override def matches(decl: Decl): Option[Env] = {
//        decl match {   
//            case VarDecl(name, typeName, value)  => 
//                var typeDecl: String = ""
//                typeNameDecl match {
//                    case Some(value) => if(typeName != value) return None else typeDecl = value 
//                    case _           => 
//                }
//                
//                val nameEnv = varName.matchDeclName(name, typeDecl, decl.id.get)
//                val resEnv: Option[Env] = (value,valueDecl) match {
//                    case (Some(expr), Some(exprDecl)) => Some(nameEnv & exprDecl.matchEnv(expr))  
//                    case (None, None)                 => Some(nameEnv & new BindingsEnv)    
//                    case (_, _)                       => None
//                }
//                    
//                resEnv match {
//                    case Some(BindingsEnv(bind)) => resEnv
//                    case _                       => None
//                }
//            case _  => None
//        }
//    }
//}