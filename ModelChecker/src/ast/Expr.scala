package ast

abstract class Expr
case class BinaryOp(left: Expr, right: Expr) extends Expr
case class UnaryOp (op: Expr               ) extends Expr
case class Variable(x: String              ) extends Expr
case class Litteral(x: AnyVal              ) extends Expr

