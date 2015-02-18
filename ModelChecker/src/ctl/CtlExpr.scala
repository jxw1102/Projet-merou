package ctl

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used in CTL 
sealed abstract class CtlExpr[M <: MetaVariable, N, V <: Value] {
    type Ctl = CtlExpr[M,N,V]
    
    def AU(that: Ctl) = _AU(this,that)
    def EU(that: Ctl) = _EU(this,that)
    def &&(that: Ctl) = And(this,that)
    def ||(that: Ctl) = Or (this,that)
    def unary_!                = Not(this)
}

// Binary operators
final case class And   [M <: MetaVariable, N, V <: Value](left: CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V] 
final case class Or    [M <: MetaVariable, N, V <: Value](left: CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V]
final case class _AU   [M <: MetaVariable, N, V <: Value](left: CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V]
final case class _EU   [M <: MetaVariable, N, V <: Value](left: CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V]

// Unary operators
final case class AX    [M <: MetaVariable, N, V <: Value](op  : CtlExpr[M,N,V])                        extends CtlExpr[M,N,V] 
final case class EX    [M <: MetaVariable, N, V <: Value](op  : CtlExpr[M,N,V])                        extends CtlExpr[M,N,V]
//final case class AG    [N,T](op  : Ctl)                      extends Ctl 
//final case class EG    [N,T](op  : Ctl)                      extends Ctl 
//final case class AF    [N,T](op  : Ctl)                      extends Ctl 
//final case class EF    [N,T](op  : Ctl)                      extends Ctl 
final case class Not      [M <: MetaVariable, N,V <: Value](op    : CtlExpr[M,N,V])                     extends CtlExpr[M,N,V]
final case class Exists   [M <: MetaVariable, N,V <: Value](typeOf: TypeOf[M, V], op  : CtlExpr[M,N,V]) extends CtlExpr[M,N,V]
final case class Predicate[M <: MetaVariable, N,V <: Value](label : Labelizer[M,N,V])                   extends CtlExpr[M,N,V]

abstract class Labelizer[M <: MetaVariable, N,V <: Value] {
    type Env = Environment[M, V]
    
    def test(t: N): Option[Environment[M, V]]
}

abstract class TypeOf[M <: MetaVariable, V](val varName: M) {
    def cast(n: V): Boolean
    def filter(set: Set[V]) = set.filter(cast)
}
final case class NoType[M <: MetaVariable, V](name: M) extends TypeOf[M, V](name) {
    def cast(n: V) = true
}
