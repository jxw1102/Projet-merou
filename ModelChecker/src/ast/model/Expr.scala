package ast.model

/**
 * Classes used to represent expressions within the program
 * @author Sofia Boutahar
 */
sealed abstract class Expr(typeOf: String) extends ForInitializer {
    val getType   = typeOf
    def isPointer = typeOf.last == '*' 
    
    def matches(that: Expr): Boolean = (this,that) match {
        case (BinaryOp(_,xl,xr,xo),BinaryOp(_,yl,yr,yo))                 => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (UnaryOp(_,xopd,xopr,xkind),UnaryOp(_,yopd,yopr,ykind))     => (xkind == ykind) && (xopr == yopr) && (xopd matches yopd)
        case (CompoundAssignOp(_,xl,xr,xo),CompoundAssignOp(_,yl,yr,yo)) => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (Literal(_,x,u),Literal(_,y,v))                             => x == y && u == v
        case (DeclRefExpr(_,_,x,_,_),DeclRefExpr(_,_,y,_,_))             => x == y
        case (ConditionalOperator(_,x,_),ConditionalOperator(_,y,_))     => (x._1 matches y._1) && (x._2 matches y._2) && (x._3 matches y._3)
        case (ArraySubscriptExpr(_,x),ArraySubscriptExpr(_,y))           => (x._1 matches y._1) && (x._2 matches y._2)
        case (InitListExpr(_,x),InitListExpr(_,y))                       => x.zip(y).forall(p => p._1 matches p._2)
        case (CallExpr(_,x),CallExpr(_,y))                               => x.zip(y).forall(p => p._1 matches p._2)
        case _                                                           => false
    }
    
    private def formatBinary(s1: Any, s2: String, s3: Any) = "(%s %s %s)".format(s1,s2,s3)
    override def toString = this match {
        case ConditionalOperator(_,(x,y,z),_)  => "(%s ? %s : %s)".format(x,y,z)
        case CompoundAssignOp   (_,l,r,op)     => formatBinary(l,op,r)
        case BinaryOp           (_,l,r,op)     => formatBinary(l,op,r)
        case UnaryOp            (_,x,op,pos)   => (if (pos == Prefix) List(op,x) else List(x,op)).mkString
        case Literal            (_,_,y)        => y
        case ArraySubscriptExpr (_,(x,y))      => "%s[%s]".format(x,y)
        case InitListExpr       (_,exprs)      => exprs.mkString("{ ",","," }")
        case DeclRefExpr        (_,_,x,_,_)    => x
        case CallExpr           (_,params)     => params.head match {
            case DeclRefExpr    (_,_,name,_,_) => "%s(%s)".format(name,params.drop(1).mkString(","))
            case _ => throw new IllegalStateException("The first child of a CallExpr should always be a DeclRefExpr")
        }
    }
}
final case class BinaryOp           (typeOf: String, left: Expr, right: Expr, operator: String)                                 extends Expr(typeOf)
final case class UnaryOp            (typeOf: String, operand: Expr, operator: String, pos: OpPosition)                          extends Expr(typeOf)
final case class CompoundAssignOp   (typeOf: String, left: Expr, right: Expr, operator: String)                                 extends Expr(typeOf)
final case class Literal            (typeOf: String, typeName: String, value: String)                                           extends Expr(typeOf)
final case class DeclRefExpr        (typeOf: String, targetType: String, targetName: String, targetId: String, refType: String) extends Expr(typeOf)
final case class ConditionalOperator(typeOf: String, exprs: (Expr,Expr,Expr), returnType: String)                               extends Expr(typeOf)
final case class ArraySubscriptExpr (typeOf: String, exprs: (Expr, Expr))                                                       extends Expr(typeOf)
final case class InitListExpr       (typeOf: String, exprs: List[Expr])                                                         extends Expr(typeOf)
final case class CallExpr           (typeOf: String, params: List[Expr])                                                        extends Expr(typeOf) {
 val funcDeclExpr = params.head
}

sealed abstract class OpPosition 
object OpPosition {
    def apply(kind: String) = if (kind == "prefix") Prefix else Postfix
}
final case object Postfix extends OpPosition
final case object Prefix  extends OpPosition

