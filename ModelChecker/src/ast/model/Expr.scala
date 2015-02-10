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
        case (UnaryOp(xopd,xopr,xkind),UnaryOp(yopd,yopr,ykind))     => (xkind == ykind) && (xopr == yopr) && (xopd matches yopd)
        case (CompoundAssignOp(xl,xr,xo),CompoundAssignOp(yl,yr,yo)) => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (Literal(x,u),Literal(y,v))                             => x == y && u == v
        case (DeclRefExpr(_,_,x,_),DeclRefExpr(_,_,y,_))             => x == y
        case (ConditionalOperator(x,_),ConditionalOperator(y,_))     => (x._1 matches y._1) && (x._2 matches y._2) && (x._3 matches y._3)
        case (ArraySubscriptExpr(x),ArraySubscriptExpr(y))           => (x._1 matches y._1) && (x._2 matches y._2)
        case (InitListExpr(x),InitListExpr(y))                       => x.zip(y).forall(p => p._1 matches p._2)
        case (CallExpr(_,x),CallExpr(_,y))                           => x.zip(y).forall(p => p._1 matches p._2)
        case _                                                       => false
    }
    
    private def formatBinary(s1: Any, s2: String, s3: Any) = "(%s %s %s)".format(s1,s2,s3)
    override def toString = this match {
    	case ConditionalOperator((x,y,z),_) => "(%s ? %s : %s)".format(x,y,z)
    	case CompoundAssignOp   (l,r,op)    => formatBinary(l,op,r)
        case BinaryOp           (l,r,op)    => formatBinary(l,op,r)
        case UnaryOp            (x,op,pos)  => (if (pos == Prefix) List(op,x) else List(x,op)).mkString
        case Literal            (_,y)       => y
        case ArraySubscriptExpr ((x,y))     => "%s[%s]".format(x,y)
        case InitListExpr       (exprs)     => exprs.mkString("{ ",","," }")
        case DeclRefExpr        (_,x,_,_)   => x
        case CallExpr           (_,params)  => params.head match {
            case DeclRefExpr(_,name,_,_) => "%s(%s)".format(name,params.drop(1).mkString(","))
        }
    }
}
final case class BinaryOp           (left: Expr, right: Expr, operator: String)                                 extends Expr
final case class UnaryOp            (operand: Expr, operator: String, pos: OpPosition)                          extends Expr
final case class CompoundAssignOp   (left: Expr, right: Expr, operator: String)                                 extends Expr
final case class Literal            (typeName: String, value: String)                                           extends Expr
final case class DeclRefExpr        (targetType: String, targetName: String, targetId: String, refType: String) extends Expr
final case class ConditionalOperator(exprs: (Expr,Expr,Expr), returnType: String)                               extends Expr
final case class ArraySubscriptExpr (exprs: (Expr, Expr))                                                       extends Expr
final case class InitListExpr       (exprs: List[Expr])                                                         extends Expr
final case class CallExpr           (returnType: String, params: List[Expr])                                    extends Expr {
    val funcDeclExpr = params.head
}

sealed abstract class OpPosition 
object OpPosition {
    def apply(kind: String) = if (kind == "prefix") Prefix else Postfix
}
final case object Postfix extends OpPosition
final case object Prefix  extends OpPosition

