package ast

abstract class Expr
case class BinaryOp(left: Expr, right: Expr) extends Expr
case class UnaryOp(op: Expr) extends Expr