package ast

sealed abstract class Expr extends Stmt
final case class BinaryOp(left: Expr, right: Expr) extends Expr
final case class UnaryOp (op: Expr               ) extends Expr
final case class Variable(x: String              ) extends Expr
final case class Litteral(x: String              ) extends Expr
final case class DeclRefExpr(typeName: String, varName: String) extends Expr