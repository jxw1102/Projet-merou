package cfg

import ast.ProgramNodeLabelizer
import ast.For
import ast.While
import ast.Identifier
import ast.Expression
import ast.If
import ast.Assignment
import ast.model.Expr
import ast.For
import ast.While
import ast.Assignment
import ast.model.BinaryOp

/**
 * @author Zohour Abouakil
 */

trait ExprPattern {
    def matches(expr: Expr): Boolean
}

case class BinaryOpPattern (left: Option[Expr], right: Option[Expr], op: BinaryOp){
   // def matches()
    //8: 
}

class IfLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = ifNode match { case If(expr,_,_) => pattern.matches(expr) }
    def visitFor       (forNode  : For       ) = false
    def visitWhile     (whileNode: While     ) = false
    def visitAssignment(stmt     : Assignment) = false
    def visitIdentifier(id       : Identifier) = false
    def visitExpression(expr     : Expression) = false
}

class ForLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = false
    def visitFor       (forNode  : For       ) = forNode match { case For(expr,_,_) => pattern.matches(expr) }
    def visitWhile     (whileNode: While     ) = false
    def visitAssignment(stmt     : Assignment) = false
    def visitIdentifier(id       : Identifier) = false
    def visitExpression(expr     : Expression) = false
}

class WhileLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = false
    def visitFor       (forNode  : For       ) = false
    def visitWhile     (whileNode: While     ) = whileNode match { case While(expr,_,_) => pattern.matches(expr) }
    def visitAssignment(stmt     : Assignment) = false
    def visitIdentifier(id       : Identifier) = false
    def visitExpression(expr     : Expression) = false
}

class AssignementLabelizer(val pattern: ExprPattern) extends ProgramNodeLabelizer {
    def visitIf        (ifNode   : If        ) = false
    def visitFor       (forNode  : For       ) = false
    def visitWhile     (whileNode: While     ) = false
    def visitAssignment(stmt     : Assignment) = stmt match { case Assignment(expr,_,_) => pattern.matches(expr) }
    def visitIdentifier(id       : Identifier) = false
    def visitExpression(expr     : Expression) = false
}