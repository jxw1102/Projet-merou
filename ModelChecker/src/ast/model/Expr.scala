package ast.model

/**
 * Classes used to represent expressions within the program
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
sealed abstract class Expr extends Stmt with ForInitializer {
    def matches(that: Expr): Boolean = (this,that) match {
        case (BinaryOp(xl,xr,xo),BinaryOp(yl,yr,yo))                 => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (UnaryOp(xopd,xopr),UnaryOp(yopd,yopr))                 => (xopd matches yopd) && (xopr matches yopr)
        case (CompoundAssignOp(xl,xr,xo),CompoundAssignOp(yl,yr,yo)) => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (Literal(x),Literal(y))                                 => x == y
        case (DeclRefExpr(_,_,x,_),DeclRefExpr(_,_,y,_))             => x == y
        case (ConditionalOperator(x,_),ConditionalOperator(y,_))     => (x._1 matches y._1) && (x._2 matches y._2) && (x._3 matches y._3)
        case (ArraySubscriptExpr(x),ArraySubscriptExpr(y))           => (x._1 matches y._1) && (x._2 matches y._2)
        case (InitListExpr(x),InitListExpr(y))                       => x.zip(y).forall(p => p._1 matches p._2)
        case (CallExpr(_,x),CallExpr(_,y))                           => x.zip(y).forall(p => p._1 matches p._2)
        case _ => false
    }
}
final case class BinaryOp           (left: Expr, right: Expr, operator: String)                                 extends Expr
final case class UnaryOp            (operand: Expr, operator: String)                                           extends Expr
final case class CompoundAssignOp   (left: Expr, right: Expr, operator: String)                                 extends Expr
final case class Literal            (x: String)                                                                 extends Expr
final case class DeclRefExpr        (targetType: String, targetName: String, targetId: String, refType: String) extends Expr
final case class ConditionalOperator(exprs: (Expr,Expr,Expr), returnType: String)                               extends Expr
final case class ArraySubscriptExpr (exprs: (Expr, Expr))                                                       extends Expr
final case class InitListExpr       (exprs: List[Expr])                                                         extends Expr
final case class CallExpr           (returnType: String, params: List[Expr])                                    extends Expr {
    val funcDeclExpr = params.head
}
