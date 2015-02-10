package cfg


import ast.Expression
import ast.For
import ast.Identifier
import ast.If
import ast.ProgramNodeLabelizer
import ast.Statement
import ast.While
import ast.model._
import ctl.Bindings
import ctl.Bottom
import ctl.Environment

/**
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 */

sealed abstract class PatternExpr
case class UndefinedExpr(name: String) extends PatternExpr
case class DefinedExpr  (expr: Expr  ) extends PatternExpr

trait ExprPattern {
    def matches(expr: Expr): Option[Environment]
    
    def matchEnv(pattern: PatternExpr, expr: Expr) = pattern match {
            case DefinedExpr  (e   : Expr  ) => if (e matches expr) new Bindings else Bottom
            case UndefinedExpr(name: String) => new Bindings(Map(name -> expr))
    }
}

// This class works for BinaryOp and CompoundAssignOp
case class BinaryOpPattern (left: PatternExpr, right: PatternExpr, op: String) extends ExprPattern{   
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case BinaryOp(l,r,operator) => 
              if (operator == op) {
                  val lenv = matchEnv(left ,l)
                  val renv = matchEnv(right,r)
                  (lenv.unapply,renv.unapply) match {
                      case (Some((lpos,_)),Some((rpos,_))) => Some(new Bindings(lpos ++ rpos))
                      case _                               => None
                  } 
              }
              else     
                  None
          case _ => None
        }
    }
}

case class UnaryOpPattern (operand: PatternExpr, op: String, kind: OpPosition) extends ExprPattern{
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case UnaryOp(operand,operator,kind) =>  
              if (operator == op && this.kind == kind) {
                  val env = matchEnv(this.operand, operand)
                  env.unapply match {
                      case Some((pos,_)) => Some(new Bindings(pos))
                      case _             => None
                  }
              }
              else 
                  None
          case _ => None
        }
    }
}

case class LiteralPattern(typeName: String, value: String) extends ExprPattern {
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case Literal(tp,vl) =>  
              if (typeName == tp && value == vl) {
                  Some(new Bindings())
              } else {
                  None
              }
          case _ => None
        }
    }
}

case class DeclRefExprPattern(id: String, refType: String) extends ExprPattern {
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case DeclRefExpr(_,_,id,refType) =>  
              if (this.id == id && this.refType == refType) {
                Some(new Bindings())
              } else {
                  None
              }
          case _ => None
        }
    }
}

// patternExprs: (cond,yes,no)
case class ConditionalOperatorPattern(patternExprs: (PatternExpr,PatternExpr,PatternExpr), rtnType: String) extends ExprPattern {
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case ConditionalOperator(exprs,rtnType) =>  
              if (this.rtnType == rtnType) {
                  val condEnv = matchEnv(patternExprs._1,exprs._1)
                  val yesEnv  = matchEnv(patternExprs._2,exprs._2)
                  val noEnv   = matchEnv(patternExprs._3,exprs._3)
                  (condEnv.unapply,yesEnv.unapply,noEnv.unapply) match {
                      case (Some((cpos,_)),Some((ypos,_)),Some((npos,_))) => Some(new Bindings(cpos ++ ypos ++ npos))
                      case _ => None
                  }
              }
              else 
                  None
          case _ => None
        }
    }
}

// patternExprs: (array,index)
case class ArraySubscriptExprPattern(patternExprs: (PatternExpr,PatternExpr)) extends ExprPattern {
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case ArraySubscriptExpr(exprs) => 
              val arrayEnv = matchEnv(patternExprs._1,exprs._1)
              val indexEnv = matchEnv(patternExprs._2,exprs._2)
              (arrayEnv.unapply,indexEnv.unapply) match {
                  case (Some((apos,_)),Some((ipos,_))) => Some(new Bindings(apos ++ ipos))
                  case _ => None
              }
          case _ => None
        }
    }
}

/*
case class CallExprPattern(params: List[PatternExpr], rtnType: String) extends ExprPattern {
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case CallExpr(rtnType,params) =>  
              if (this.rtnType == rtnType) {
                  
              }
              else 
                  None
          case _ => None
        }
    }
}
*/

//case class InitListExpr       (exprs: List[Expr])                                                         extends Expr
//case class CallExpr           (returnType: String, params: List[Expr])                                    extends Expr {

class IfLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitIf(ifNode   : If) = ifNode match { 
        case If(expr,_,_) => pattern.matches(expr) 
        case _            => None
    } 
}

class ForLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitFor(forNode  : For) = forNode match {
            case For(None      ,_,_) => None
            case For(Some(expr),_,_) => pattern.matches(expr) 
        }
}

class WhileLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitWhile(whileNode: While) = whileNode match { case While(expr,_,_) => pattern.matches(expr) }
}

class StatementLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitExpression(expr: Expression) = pattern.matches(expr.e)
}

class IdentifierLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitIdentifier(id: Identifier) = id match { case Identifier(s,_,_)  => None }
}

class ExpressionLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitExpression(expr: Expression) = expr match { case Expression(e,_,_) => pattern.matches(e) }
}