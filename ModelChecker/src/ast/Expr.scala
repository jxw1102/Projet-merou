package ast

/**
 * Classes used to represent expressions within the program
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
sealed abstract class Expr extends Stmt with ForInitializer
final case class BinaryOp(left: Expr, right: Expr, operator: String) extends Expr
final case class UnaryOp (operand: Expr, operator: String) extends Expr
final case class CompoundAssignOp(left: Expr, right: Expr, operator: String) extends Expr
final case class Literal(x: String              ) extends Expr
final case class DeclRefExpr(targetType: String, targetName: String, targetId: Long, refType: String) extends Expr
final case class CallExpr(returnType: String, params: List[Expr]) extends Expr {
    val funcDeclExpr = params.head
}
final case class ConditionalOperator(exprs: (Expr,Expr,Expr), returnType: String) extends Expr {
    // is it really useful as we can get them from the pattern ?
    val condition = exprs._1
    val yes       = exprs._2
    val no        = exprs._3
}
