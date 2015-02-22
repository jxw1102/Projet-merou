package cfg

import ast.Expression
import ast.For
import ast.Identifier
import ast.If
import ast.Statement
import ast.While
import ast.model._
import ctl.Bottom
import ctl.Environment
import ctl.ConvertEnv
import ctl.MetaVariable
import ctl.Value
import ctl.BindingsEnv
import com.sun.xml.internal.bind.v2.schemagen.episode.Bindings
import ctl.Labelizer
import ast.ProgramNode
import ctl.TypeOf
import ctl.MetaVarBinding
import ctl.NegBinding
import ctl.PosBinding

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

// This class works for BinaryOp and CompoundAssignOp
case class BinaryOpPattern (left: Pattern, right: Pattern, op: String) extends ExprPattern {   
    override def matches(expr: Expr): Option[Env] = {
        expr match {
          case BinaryOp(l,r,operator) => 
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
          case UnaryOp(operand,operator,kind) =>  
              if (operator == op && this.kind == kind) {
                  val env = this.operand.matchEnv(operand)
                  env match {
                      case BindingsEnv(_) => Some(env)
                      case _                 => None
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
                      case Some(value) => if (this.rtnType == rtnType) matchesParams(paramsFun) else None
                      case _           => matchesParams(paramsFun)
                  } 
              }
              else 
                  None
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


class StatementLabelizer(val pattern: DeclPattern) extends Labelizer[CFGMetaVar, ProgramNode, CFGVal] {
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