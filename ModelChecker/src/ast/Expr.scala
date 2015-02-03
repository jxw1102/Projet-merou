package ast

sealed abstract class Expr extends Stmt with ForInitializer
final case class BinaryOp(left: Expr, right: Expr, operator: String) extends Expr
final case class UnaryOp (operand: Expr, operator: String) extends Expr
final case class CompoundAssignOp(left: Expr, right: Expr, operator: String) extends Expr
final case class Variable(x: String              ) extends Expr
final case class Litteral(x: String              ) extends Expr
final case class DeclRefExpr(targetType: String, targetName: String, targetId: Long, refType: String) extends Expr
final case class CallExpr(returnType: String, params: List[Expr]) extends Expr {
    def funcDeclExpr = params(0)
}
final case class ConditionalOperator(exprs: List[Expr], returnType: String) extends Expr {
    def condition = exprs(0)
    def yes = exprs(1)
    def no = exprs(2)
}
