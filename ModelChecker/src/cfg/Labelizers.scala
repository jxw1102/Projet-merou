package cfg

import ast.ProgramNodeLabelizer
import ast.For
import ast.While
import ast.Identifier
import ast.Expression
import ast.If
import ast.model.Expr
import ast.For
import ast.While
import ast.model.BinaryOp
import ast.Statement
import ctl.Environment
import ctl.Bindings
import scala.collection.mutable.Map
import ctl.Bottom

/**
 * @author Zohour Abouakil
 */

sealed abstract class PatternExpr
case class UndefinedExpr(name: String) extends PatternExpr
case class DefinedExpr  (expr: Expr  ) extends PatternExpr

trait ExprPattern {
    def matches(expr: Expr): Option[Environment]
}

case class BinaryOpPattern (left: PatternExpr, right: PatternExpr, op: String) {
    private def matchEnv(pattern: PatternExpr, expr: Expr) = pattern match {
            case DefinedExpr  (e   : Expr  ) => if (e == expr) new Bindings else Bottom
            case UndefinedExpr(name: String) => new Bindings(Map(name -> expr))
    }
    
    def matches(expr: Expr): Option[Environment] = {
        expr match {
          case BinaryOp(l,r,operator) =>  
              val lenv = matchEnv(left ,l)
              val renv = matchEnv(right,r)
              (lenv.unapply,renv.unapply) match {
                  case (Some((lpos,_)),Some((rpos,_))) => Some(new Bindings(lpos ++ rpos))
                  case _                               => None
              }
          case _ => None
        }
    }
}

class IfLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = ifNode match { case If(expr,_,_) => pattern.matches(expr) } 
    def visitFor       (forNode  : For       ) = None
    def visitWhile     (whileNode: While     ) = None
    def visitStatement (stmt     : Statement ) = None
    def visitIdentifier(id       : Identifier) = None
    def visitExpression(expr     : Expression) = None
}

class ForLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = None
    def visitFor       (forNode  : For       ) = forNode match {
            case For(None      ,_,_) => None
            case For(Some(expr),_,_) => None//pattern.matches(expr) 
        }
    def visitWhile     (whileNode: While     ) = None
    def visitStatement (stmt      : Statement) = None
    def visitIdentifier(id       : Identifier) = None
    def visitExpression(expr     : Expression) = None
}

class WhileLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = None
    def visitFor       (forNode  : For       ) = None
    def visitWhile     (whileNode: While     ) = whileNode match { case While(expr,_,_) => pattern.matches(expr) }
    def visitStatement (stmt     : Statement ) = None
    def visitIdentifier(id       : Identifier) = None
    def visitExpression(expr     : Expression) = None
}

class StatementLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = None
    def visitFor       (forNode  : For       ) = None
    def visitWhile     (whileNode: While     ) = None
    def visitStatement (stmt     : Statement ) = None //whileNode match { case While(expr,_,_) => None }//pattern.matches(expr) }
    def visitIdentifier(id       : Identifier) = None
    def visitExpression(expr     : Expression) = None
}

class IdentifierLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = None
    def visitFor       (forNode  : For       ) = None
    def visitWhile     (whileNode: While     ) = None
    def visitStatement (stmt     : Statement ) = None
    def visitIdentifier(id       : Identifier) = None // whileNode match { case While(expr,_,_) => None }//pattern.matches(expr) }
    def visitExpression(expr     : Expression) = None 
}

class ExpressionLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = None
    def visitFor       (forNode  : For       ) = None
    def visitWhile     (whileNode: While     ) = None
    def visitStatement (stmt     : Statement ) = None
    def visitIdentifier(id       : Identifier) = None 
    def visitExpression(expr     : Expression) = None // whileNode match { case While(expr,_,_) => None }//pattern.matches(expr) }
}
