package ast.model

import scala.collection.mutable.StringBuilder

/**
 * Classes used to represent expressions within the program
 * @author Sofia Boutahar
 */
sealed abstract class Expr(typeOf: String) extends ForInitializer {
    val getType   = typeOf
    def isPointer = typeOf.last == '*' 
    
    def getSubExprs = this match {
        case BinaryOp           (_,l,r,_)      => List(l,r)     
        case UnaryOp            (_,op,_,_)     => List(op)
        case CompoundAssignOp   (_,l,r,_)      => List(l,r)
        case ConditionalOperator(_,(x,y,z))    => List(x,y,z)                              
        case ArraySubscriptExpr (_,(l,r))      => List(l,r)                                 
        case InitListExpr       (_,exprs)      => exprs
        case CallExpr           (_,ref,params) => ref :: params
        case _                                 => List()
    }    
    
    def matches(that: Expr): Boolean = (this,that) match {
        case (BinaryOp(_,xl,xr,xo),BinaryOp(_,yl,yr,yo))                 => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (UnaryOp(_,xopd,xopr,xkind),UnaryOp(_,yopd,yopr,ykind))     => (xkind == ykind) && (xopr == yopr) && (xopd matches yopd)
        case (CompoundAssignOp(_,xl,xr,xo),CompoundAssignOp(_,yl,yr,yo)) => (xo == yo) && (xl matches yl) && (xr matches yr)
        case (Literal(x,u),Literal(y,v))                                 => x == y && u == v
        case (DeclRefExpr(_,x,_),DeclRefExpr(_,y,_))                     => x == y
        case (ConditionalOperator(_,x),ConditionalOperator(_,y))         => (x._1 matches y._1) && (x._2 matches y._2) && (x._3 matches y._3)
        case (ArraySubscriptExpr(_,x),ArraySubscriptExpr(_,y))           => (x._1 matches y._1) && (x._2 matches y._2)
        case (InitListExpr(_,x),InitListExpr(_,y))                       => x.zip(y).forall(p => p._1 matches p._2)
        case (CallExpr(_,x,y),CallExpr(_,z,t))                           => (x matches z) && y.zip(t).forall(p => p._1 matches p._2)
        case _                                                           => false
    }
    
    private def formatBinary(s1: Any, s2: String, s3: Any) = "(%s %s %s)".format(s1,s2,s3)
    override def toString = this match {
        case ConditionalOperator(_,(x,y,z))    => "(%s ? %s : %s)".format(x,y,z)
        case CompoundAssignOp   (_,l,r,op)     => formatBinary(l,op,r)
        case BinaryOp           (_,l,r,op)     => formatBinary(l,op,r)
        case UnaryOp            (_,x,op,pos)   => (if (pos == Prefix) List(op,x) else List(x,op)).mkString
        case Literal            (_,y)          => y
        case ArraySubscriptExpr (_,(x,y))      => "%s[%s]".format(x,y)
        case InitListExpr       (_,exprs)      => exprs.mkString("{ ",","," }")
        case DeclRefExpr        (_,x,_)        => x
        case CallExpr           (_,ref,params) => "%s(%s)".format(ref.targetName,params.mkString(","))
        case MemberExpr         (_,t,m)        => "%s%s".format(t,m)
        case UnaryExprOrTypeTraitExpr(_,v,e)   => e match { case Some(ex) => "%s(%s)".format(v,ex); case None => v }
        case CXXNewExpr(t,c)                   => val tp = t.substring(0, t.length()-2); c match { case Some(cnt) => "new %s[%s]".format(tp,cnt); case None => "new %s".format(tp) }
        case CXXDeleteExpr(_,c)                => "delete %s".format(c)
        case CXXConstructExpr(_,s,p)           => "%s%s".format(s,p.mkString("(", ",", ")"))
        case CXXTemporaryObjectExpr(_,p)       => p.mkString("(", ",", ")")
        case CXXOperatorCallExpr(_,o,l,r)      => "%s %s %s".format(l,o,r)
    }
}
final case class BinaryOp           (typeOf: String, left: Expr, right: Expr, operator: String)        extends Expr(typeOf)
final case class UnaryOp            (typeOf: String, operand: Expr, operator: String, pos: OpPosition) extends Expr(typeOf)
final case class CompoundAssignOp   (typeOf: String, left: Expr, right: Expr, operator: String)        extends Expr(typeOf)
final case class Literal            (typeOf: String, value: String)                                    extends Expr(typeOf)
final case class DeclRefExpr        (typeOf: String, targetName: String, targetId: String)             extends Expr(typeOf)
final case class ConditionalOperator(typeOf: String, exprs: (Expr,Expr,Expr))                          extends Expr(typeOf)
final case class ArraySubscriptExpr (typeOf: String, exprs: (Expr, Expr))                              extends Expr(typeOf)
final case class InitListExpr       (typeOf: String, exprs: List[Expr])                                extends Expr(typeOf)
final case class CallExpr           (typeOf: String, ref: DeclRefExpr, params: List[Expr])             extends Expr(typeOf)
final case class MemberExpr         (typeOf: String, target: Expr, member: String)                     extends Expr(typeOf)
final case class UnaryExprOrTypeTraitExpr(typeOf: String, value: String, expr: Option[Expr])           extends Expr(typeOf)
final case class CXXNewExpr         (typeOf: String, count: Option[Expr])                              extends Expr(typeOf)
final case class CXXDeleteExpr      (typeOf: String, target: Expr)                                     extends Expr(typeOf)
final case class CXXConstructExpr   (typeOf: String, source: String, params: List[Expr])               extends Expr(typeOf)
final case class CXXTemporaryObjectExpr(typeOf: String, params: List[Expr])                            extends Expr(typeOf)
final case class CXXOperatorCallExpr(typeOf: String, operator: Expr, left: Expr, right: Expr)          extends Expr(typeOf)

sealed abstract class OpPosition 
object OpPosition {
    def apply(kind: String) = if (kind == "prefix") Prefix else Postfix
}
final case object Postfix extends OpPosition
final case object Prefix  extends OpPosition
