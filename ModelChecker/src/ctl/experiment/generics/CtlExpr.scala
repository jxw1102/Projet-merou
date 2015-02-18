package ctl.experiment.generics

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used in CTL 
sealed abstract class CtlExpr[N,T] {
    def AU(that: CtlExpr[N,T]) = _AU(this,that)
    def EU(that: CtlExpr[N,T]) = _EU(this,that)
    def &&(that: CtlExpr[N,T]) = And(this,that)
    def ||(that: CtlExpr[N,T]) = Or (this,that)
    def unary_!                = Not(this)
}

// Binary operators
final case class And   [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class Or    [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class _AU   [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class _EU   [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 

// Unary operators
final case class AX    [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
final case class EX    [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
//final case class AG    [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
//final case class EG    [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
//final case class AF    [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
//final case class EF    [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
final case class Not   [N,T](op  : CtlExpr[N,T])                      extends CtlExpr[N,T] 
final case class Exists[N,T]  (typeOf: TypeOf[T], op  : CtlExpr[N,T]) extends CtlExpr[N,T]
final case class Predicate[N,T]  (label: Labelizer[N,T])              extends CtlExpr[N,T]

abstract class Labelizer[N,T] {
    def test(t: N): Option[Environment[T]]
}

abstract class TypeOf[T](val varName: String) {
    def cast(n: T): Boolean
    def filter(set: Set[T]) = set.filter(cast)
}
final case class NoType[T](name: String) extends TypeOf[T](name) {
    def cast(n: T) = true
}
