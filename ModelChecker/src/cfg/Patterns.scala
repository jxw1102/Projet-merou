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
import ast.model.Literal
import ctl.Top

/**
 * This file contains all the Pattern definitions for our CFG. Patterns enable to analyze the expression(s)
 * held by a CFGNode in order to extract (or not) environments for which a pattern is matched. They are 
 * intensively used by the labelizers.
 * @author Zohour Abouakil
 * @author David Courtinot
 */

////////////////////////////////////////////////////////////////////////////////
//                                BASE PATTERNS
////////////////////////////////////////////////////////////////////////////////

sealed trait Pattern extends ConvertEnv {
    type Env = Environment[CFGMetaVar,CFGVal]
}

/**
 * Represents any class able to tell whether a Decl object matches a DeclPattern or not.
 */
sealed trait DeclPattern extends Pattern {
	def matches(decl: Decl): Option[Env]
}

/**
 * Represents any class able to tell whether a String object matches a StringPattern or not.
 */
sealed trait StringPattern extends Pattern {
	def matches(s: String): Option[Env]
}

/**
 * Represents any class able to tell whether an Expr object matches an ExprPattern or not.
 */
sealed trait ExprPattern extends Pattern {
    def matches (expr: Expr): Option[Env] 
}

object ExprPattern {
    type Env = Environment[CFGMetaVar,CFGVal]
    
    /**
     * Computes the intersection of the result of two matchers.
     * @note This will return a non-None result iff both arguments are defined and contain compatible
     *       environments
     */
    def intersection(e0: Option[Env], e1: Option[Env]) = (e0,e1) match {
        case (Some(x),Some(y)) => val inter = x & y; inter match { case BindingsEnv(_) => Some(inter) case _ => None }
        case _                 => None
    }
}

// Right now, we are not willing to make ExprPattern a recursive structure
// The aim of this trait is to avoid that by allowing only them to be composed
// to make composed patterns (one level of recursivity).
// To allow deeper recursivity, just replace AtomicExprPattern with ExprPattern everywhere
// you want to allow it.
trait AtomicExprPattern extends ExprPattern

/**
 * Represents the most general Pattern : it matches anything, returning a positive binding between
 * a given meta-variable and the matched value.
 */
case class UndefinedVar (name: CFGMetaVar) extends AtomicExprPattern with DeclPattern with StringPattern {
    override def matches(expr: Expr  ) = Some(new BindingsEnv ++ (name -> expr))
    override def matches(decl: Decl  ) = Some(new BindingsEnv ++ (name -> decl))
    override def matches(s   : String) = Some(new BindingsEnv ++ (name -> s))
}

/**
 * Matches an expression iff it exactly matches the expression used to construct the DefinedExpr object
 */
case class DefinedExpr  (expr: Expr) extends AtomicExprPattern {
    override def matches(e: Expr) = if (e matches expr) Some(Top) else None
}

/**
 * Matches a String iff it is equal to the String used to construct the DefinedString object
 */
case class DefinedString(name: String) extends StringPattern {
    override def matches(s: String) = if (s == name) Some(Top) else None
}

/**
 * Matches a String iff it different to all the elements of the Set[String] used to construct the 
 * NotString object
 */
case class NotString    (not: Set[String]=Set()) extends StringPattern {
    override def matches(s: String) = if (not contains s) None else Some(Top)
}
object NotString {
    def apply(not: String*): NotString = NotString(Set(not: _*))
}

////////////////////////////////////////////////////////////////////////////////
//                                ADVANCED PATTERNS
////////////////////////////////////////////////////////////////////////////////

/**
 * Matches any expression which all leaves are literals.
 */
case class LiteralExprPattern(metavar: UndefinedVar) extends AtomicExprPattern {  
    private def forallLiteral(exprs: List[Expr]) = if (exprs.isEmpty) false else exprs.forall(isAllLiteral(_))
    
    // we need to treat CallExpr separately because its sub-expressions list contains the DeclRefExpr of
    // the function identifier, that we do not want to consider here
    private def isAllLiteral(expr: Expr): Boolean = expr match {
        case Literal(_,_)       => true
        case CallExpr(_,_,args) => forallLiteral(args)
        case _                  => forallLiteral(expr.getSubExprs)
    }
    override def matches(expr: Expr) = if (isAllLiteral(expr)) Some(Top ++ (metavar.name -> expr)) else None
}

/**
 * Matches any BinaryOp expression which operands and operator symbol match a given pattern.
 */
case class BinaryOpPattern(left: AtomicExprPattern, right: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern { 
    import ExprPattern._
    override def matches(expr: Expr) = expr match {
    	case BinaryOp(_,l,r,operator) => intersection(intersection(op.matches(operator),left.matches(l)),right.matches(r))
        case _                        => None
   }
}

/**
 * Matches any CompoundAssignOp expression which operands and operator symbol match a given pattern.
 */
case class CompoundAssignOpPattern(left: AtomicExprPattern, right: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern { 
	import ExprPattern._
	override def matches (expr: Expr) = expr match {
		case CompoundAssignOp(_,l,r,operator) => intersection(intersection(op.matches(operator),left.matches(l)),right.matches(r))
		case _                                => None
	}
}

/**
 * Matches any assignment expression which operands and operator symbol match a given pattern.
 * @pre if op is a DefinedString, passing any other value than "=", "+=", "-=", "*=", "/=" will result
 *      in an IllegalArgumentException to be thrown
 */
case class AssignmentPattern(left: AtomicExprPattern, right: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern {
    import ExprPattern._
    private val patterns: List[ExprPattern] = op match {
        case DefinedString("=")                       => List(BinaryOpPattern        (left,right,op))
        case DefinedString("+=" | "-=" | "*=" | "/=") => List(CompoundAssignOpPattern(left,right,op))
        case DefinedString(_)                         => throw new IllegalArgumentException(op + " is not an assignment operator")
        case _                                        => List(BinaryOpPattern        (left,right,DefinedString("=")),
                											  CompoundAssignOpPattern(left,right,op))
    }
    override def matches(expr: Expr): Option[Environment[CFGMetaVar,CFGVal]] = 
        patterns.map(_.matches(expr)).foldLeft[Option[Env]](None)((acc,x) => acc orElse x)
}


/**
 * Matches any assignment expression which operands and operator symbol match a given pattern.
 */
case class UnaryOpPattern(operand: AtomicExprPattern, op: StringPattern=NotString()) extends ExprPattern {
    override def matches(expr: Expr) = expr match {
    	case UnaryOp(_,operand,operator,_) => ExprPattern.intersection(op.matches(operator),this.operand.matches(operand))
        case _                             => None
   }
}

/**
 * Matches any expression which has a pointer type.
 */
case class PointerExperPattern(metavar: UndefinedVar) extends ExprPattern {
    override def matches (expr: Expr) = if (expr.isPointer) Some(Top ++ (metavar.name -> expr)) else None
}

/**
 * Matches any CallExpr expression which name, parameters and type match a given pattern.
 * @note matching the parameters is optional. If you only want to base the matching on the name
 *       and type, you should pass None for params
 */
case class CallExprPattern(
        name : StringPattern, 
        params: Option[List[AtomicExprPattern]]=None,
        typeOf: StringPattern=NotString()) extends ExprPattern {
    
    private def matchesParams(paramsFun : List[Expr]): Option[Env] = params match {
        case None        => Some(Top)
        case Some(value) =>
            if (paramsFun.size != value.size) None 
            else value.zip(paramsFun).foldLeft[Option[Env]](Some(Top)) { 
                case (acc,(pe,e)) => if (acc.isEmpty) None else ExprPattern.intersection(acc,pe.matches(e)) 
            }
    }
    
    private def matchFun(ref: DeclRefExpr): Option[Env] = (name.matches(ref.targetName),name) match {
        case (Some(_),UndefinedVar(x))  => Some(Top ++ (x -> CFGExpr(ref)))
        case (Some(_),DefinedString(_)) => Some(Top)
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

/**
 * Matches any VarDecl which name and type match a given pattern.
 * @note if there's a match and name is an UndefinedVar, it will return an environment containing a positive binding
 *       between the meta-variable held by name and a CFGVal resulting of the conversion of the matched Decl by the 
 *       conversion function passed as a parameter. This class is used to factorize the code of VarDeclPattern and
 *       VarDefPattern.
 */
sealed class VarDeclMatcher(typeOf: StringPattern, name: StringPattern)(convert: Decl => CFGVal) extends DeclPattern {
    private def matchDecl(decl: Decl): Option[Env] = (name.matches(decl.name),name) match {
        case (Some(_),UndefinedVar(x))  => Some(Top ++ (x -> convert(decl)))
        case (Some(_),DefinedString(_)) => Some(Top)
        case _                          => None
    }
    
    override def matches(decl: Decl): Option[Env] = decl match {
        case VarDecl(declName,typeName,_) =>
            typeOf match {
                case DefinedString(value) if value == typeName        => matchDecl(decl)
                case NotString    (not)   if !(not contains typeName) => matchDecl(decl)
                case UndefinedVar (x)                                 => 
                    ExprPattern.intersection(Some(Top ++ (x -> CFGString(typeName))),matchDecl(decl))
                case _                                                => None
            }
        case _ => None
    }
}

/**
 * Matches any VarDecl which name and type match a given pattern.
 * @note if there's a match and name is an UndefinedVar, it will return an environment containing a positive binding
 *       between the meta-variable held by name and a CFGDecl
 */
case class VarDeclPattern(typeOf: StringPattern, name: StringPattern)
	extends VarDeclMatcher(typeOf,name)(decl => CFGDecl(decl.id.get,decl.typeOf,decl.name))

/**
 * Matches any VarDecl which name and type match a given pattern.
 * @note if there's a match and name is an UndefinedVar, it will return an environment containing a positive binding
 *       between the meta-variable held by name and a CFGDef
 */
case class VarDefPattern(typeOf: StringPattern, name: StringPattern) 
	extends VarDeclMatcher(typeOf,name)(decl => CFGDef(decl.typeOf,decl.name))
    
//case class CXXNewExprPattern(
//        name : StringPattern, 
//        params: Option[List[AtomicExprPattern]]=None,
//        typeOf: StringPattern=NotString()) extends ExprPattern {
//    
//    private def matchesParams(paramsFun : List[Expr]): Option[Env] = params match {
//        case None        => Some(Top)
//        case Some(value) =>
//            if (paramsFun.size != value.size) None 
//            else value.zip(paramsFun).foldLeft[Option[Env]](Some(Top)) { 
//                case (acc,(pe,e)) => if (acc.isEmpty) None else ExprPattern.intersection(acc,pe.matches(e)) 
//            }
//    }
//    
//    private def matchFun(ref: DeclRefExpr): Option[Env] = (name.matches(ref.targetName),name) match {
//        case (Some(_),UndefinedVar(x))  => Some(Top ++ (x -> CFGExpr(ref)))
//        case (Some(_),DefinedString(_)) => Some(Top)
//        case _                          => None
//    }
//    
//    override def matches(expr: Expr) = expr match {
//        case CallExpr(rtn,ref,paramsFun) => typeOf.matches(rtn) match {
//            case Some(_) => ExprPattern.intersection(matchFun(ref),matchesParams(paramsFun)) 
//            case _       => None
//        } 
//        case _                           => None
//   }
//}

//case class FunctionDeclPattern(name: StringPattern, args: Option[List[ParamVarDeclPattern]]) extends DeclPattern {
//    private def matchDecl(decl: Decl): Option[Env] = (name.matches(decl.name),name) match {
//        case (Some(_),UndefinedVar(x))  => Some(new BindingsEnv ++ (x -> CFGDecl(decl.id.get,decl.typeOf,decl.name)))
//        case (Some(_),DefinedString(_)) => Some(new BindingsEnv)
//        case (None,_)    => None
//    }
//}