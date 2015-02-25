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

////////////////////////////////////////////////////////////////////////////////
//                                ADVANCED PATTERNS
////////////////////////////////////////////////////////////////////////////////

case object Anything extends ExprPattern {
    override def matches(expr: Expr): Option[Env] = Some(new BindingsEnv)
}

case class BinaryOpPattern (left: AtomicExprPattern, right: AtomicExprPattern, op: String) extends ExprPattern { 
    override def matches(expr: Expr) = expr match {
    	case BinaryOp(_,l,r,operator) if operator == op => ExprPattern.intersection(left.matches(l),right.matches(r))
        case _                                          => None
   }
}

case class CompoundAssignOpPattern (left: AtomicExprPattern, right: AtomicExprPattern, op: String) extends ExprPattern { 
    override def matches (expr: Expr) = expr match {
    	case CompoundAssignOp(_,l,r,operator) if operator == op => ExprPattern.intersection(left.matches(l),right.matches(r))
        case _                                                  => None
   }
}

case class UnaryOpPattern (operand: AtomicExprPattern, op: String, kind: OpPosition) extends ExprPattern {
    override def matches (expr: Expr) = expr match {
    	case UnaryOp (_,operand, operator, kind) if (operator == op && this.kind == kind) => this.operand.matches(operand)
        case _                                                                            => None
   }
}

case class CallExprPattern(params: List[AtomicExprPattern], rtnType: Option[String]=None) extends ExprPattern {
    def matchesParams (paramsFun : List[Expr]): Option[Env] = {
        var binding: Option[Env] = Some(new BindingsEnv)

        for(tup <- this.params.zip(paramsFun)) tup match {
            case (pe,e) => binding = ExprPattern.intersection(binding,pe.matches(e)); if (binding.isEmpty) return None
            case _      => return None
        }
        binding
    }
    
    override def matches (expr: Expr) = expr match {
    	case CallExpr (rtn, paramsFun) if (paramsFun.size == params.size)  => 
    		rtnType match {
    			case Some(value) => if (value == rtn) matchesParams(paramsFun) else None
                case _           => matchesParams(paramsFun)
            } 
        case _ => None
   }
}

case class VarDeclPattern(typeOf: Option[String], name: StringPattern) extends DeclPattern {
    private def matchDecl(decl: Decl): Option[Env] = (name.matches(decl.name),name) match {
        case (Some(_),UndefinedVar(x))  => Some(new BindingsEnv ++ (x -> CFGDecl(decl.id.get,decl.typeOf,decl.name)))
        case (Some(_),DefinedString(_)) => Some(new BindingsEnv)
        case (None,_)    => None
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

//case class FunctionDeclPattern(name: StringPattern, typeName: String, args: List[ParamVarDecl])  extends DeclPattern {
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