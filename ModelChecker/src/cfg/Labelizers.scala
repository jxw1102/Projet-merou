package cfg

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
final case class CFGDecl (ident: String) extends CFGVal {
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
    
    def matchDeclName(n: String): Env = this match {
        case DefinedDecl  (s   : String)     => if (s == n) new BindingsEnv else Bottom
        case UndefinedVar (name: CFGMetaVar) => new BindingsEnv ++ (name -> CFGDecl(n)) 
        case _                               => Bottom
    }
}

case class UndefinedVar (name: CFGMetaVar) extends Pattern
case class DefinedExpr  (expr: Expr      ) extends Pattern
case class DefinedDecl  (name: String    ) extends Pattern

trait ExprPattern extends ConvertEnv {
    type Env = Environment[CFGMetaVar,CFGVal]
    def matches(expr: Expr): Option[Env]
}

case object Anything extends ExprPattern {
    def matches(expr: Expr): Option[Env] = Some(new BindingsEnv)
}

// This class works for BinaryOp and CompoundAssignOp
case class BinaryOpPattern (left: Pattern, right: Pattern, op: String) extends ExprPattern {   
    override def matches(expr: Expr): Option[Env] = {
        expr match {
          case BinaryOp(_,l,r,operator) => 
              if (operator == op) {
                  val inter = left.matchEnv(l) & right.matchEnv(r)
                  inter match {
                      case BindingsEnv(_) => Some(inter)
                      case _              => None
                  }
              }
              else None
          case _ => None
        }
    }
}

case class UnaryOpPattern (operand: Pattern, op: String, kind: OpPosition) extends ExprPattern {
    override def matches(expr: Expr): Option[Env] = {
        expr match {
          case UnaryOp(_,operand,operator,kind) =>  
              if (operator == op && this.kind == kind) {
                  val env = this.operand.matchEnv(operand)
                  env match {
                      case BindingsEnv(_) => Some(env)
                      case _              => None
                  }
              }
              else None
          case _ => None
        }
    }
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
    
    override def matches(expr: Expr): Option[Env] = {
        expr match {
          case CallExpr(rtn,paramsFun) =>  
              if (paramsFun.size == params.size) {
                  rtnType match {
                      case Some(value) => if (value == rtn) matchesParams(paramsFun) else None
                      case _           => matchesParams(paramsFun)
                  } 
              }
              else None
          case _ => None
        }
    }
}

trait DeclPattern extends ConvertEnv {
	type Env = Environment[CFGMetaVar, CFGVal]
	def matches(decl: Decl): Option[Env]
}

case class VarDeclPattern(varName: Pattern, typeNameDecl: String, valueDecl: Option[Pattern] = None) extends DeclPattern {
	override def matches(decl: Decl) = {
        decl match {          
			case VarDecl(name, typeName, value)  => 
                if(typeName == typeNameDecl) {
                    val valueEnv: Option[Env] = (value,valueDecl) match {
                        case (Some(expr), Some(exprDecl)) => Some(exprDecl.matchEnv(expr))  
                        case (None, None)                 => Some(new BindingsEnv)    
                        case (_, _)                       => None
                    }
                    
                    val nameEnv = varName.matchDeclName(name)
                    val resEnv  = nameEnv & valueEnv.get
                    resEnv match {
                        case BindingsEnv(bind) => Some(resEnv)
                        case _                 => None
                    }
    			}
    			else None
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

case class StatementLabelizer(val pattern: DeclPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
    override def test(t: ProgramNode): Option[Environment[CFGMetaVar, CFGVal]] = {
//        println("node " + t + " ::  "+ t.getClass)
        t match {
//          case Statement(stmt: Decl,_,_)      => pattern.matches(stmt)
            case Statement(DeclStmt(decls),_,_) =>
                val listDecl = (for(d <- decls; if (d.isInstanceOf[VarDecl]);
                                resDecl=pattern.matches(d.asInstanceOf[VarDecl]); if(resDecl.isDefined))
                                yield resDecl.get)
               if(!listDecl.isEmpty){
                   val temp = listDecl.reduce(_ & _)
                   temp match {
                       case BindingsEnv(value) => Some(temp)
                       case _                  => None
                   }
               }
               else None
            case _ => None 
        }
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