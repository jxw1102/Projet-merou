package cfg

import scala.collection.mutable.Map

import ast.Expression
import ast.For
import ast.For
import ast.Identifier
import ast.If
import ast.ProgramNodeLabelizer
import ast.Statement
import ast.While
import ast.While
import ast.model.BinaryOp
import ast.model.Expr
import ast.model.Literal
import ast.model.UnaryOp
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

case class UnaryOpPattern (patternExpr: PatternExpr, op: String) extends ExprPattern{
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case UnaryOp(operand,operator,kind) =>  
              if (operator == op) {
                  val env = matchEnv(patternExpr ,operand)
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

case class LiteralPattern(lit: PatternExpr) extends ExprPattern {
    override def matches(expr: Expr): Option[Environment] = {
        expr match {
          case Literal(_,_) =>  
              val env = matchEnv(lit,expr)
              (env.unapply) match {
                  case (Some((pos,_))) => Some(new Bindings(pos))
                  case _               => None
              }
          case _ => None
        }
    }
}


//case class Literal            (x: String)  

//case class CompoundAssignOp   (left: Expr, right: Expr, operator: String)                                 extends Expr
//case class DeclRefExpr        (targetType: String, targetName: String, targetId: String, refType: String) extends Expr
//case class ConditionalOperator(exprs: (Expr,Expr,Expr), returnType: String)                               extends Expr
//case class ArraySubscriptExpr (exprs: (Expr, Expr))                                                       extends Expr
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
    override def visitIdentifier(id: Identifier) = id match { case Identifier(s, _, _)  => None }
}

class ExpressionLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    override def visitExpression(expr: Expression) = expr match { case Expression(e, _, _) => pattern.matches(e) }
}