package ast

sealed abstract class Expr extends Stmt with ForInitializer
final case class BinaryOp(left: Expr, right: Expr) extends Expr
final case class UnaryOp (op: Expr               ) extends Expr
final case class Variable(x: String              ) extends Expr
final case class Litteral(x: AnyVal              ) extends Expr