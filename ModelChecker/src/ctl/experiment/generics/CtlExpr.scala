package ctl.experiment.generics

import ast.ProgramNodeLabelizer

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used in CTL 
<<<<<<< HEAD
sealed abstract class CtlExpr[T] {
    def AU(that: CtlExpr[T])  = _AU(this,that)
    def EU(that: CtlExpr[T])  = _EU(this,that)
    def &&(that : CtlExpr[T]) = And(this,that)
    def ||(that : CtlExpr[T]) = Or (this,that)
    def unary_!            = Not(this)
}

// Binary operators
final case class And[T]  (left: CtlExpr[T], right: CtlExpr[T]) extends CtlExpr[T] 
final case class Or[T]   (left: CtlExpr[T], right: CtlExpr[T]) extends CtlExpr[T] 
final case class _AU[T]  (left: CtlExpr[T], right: CtlExpr[T]) extends CtlExpr[T] 
final case class _EU[T]  (left: CtlExpr[T], right: CtlExpr[T]) extends CtlExpr[T] 

// Unary operators
final case class AX[T]                    (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class EX[T]                    (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class AG[T]                    (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class EG[T]                    (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class AF[T]                    (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class EF[T]                    (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class Not[T]                   (op  : CtlExpr[T]) extends CtlExpr[T] 
final case class Exists[T]  (name: String, op  : CtlExpr[T]) extends CtlExpr[T]

// Predicate
final case class Predicate[T]  (label: Labelizer[T]) extends CtlExpr[T]
abstract class Labelizer[T] {
    def test(t: T): Environment[T]
}
=======
sealed abstract class CtlExpr[N,T] {
    def AU(that: CtlExpr[N,T]) = _AU(this,that)
    def EU(that: CtlExpr[N,T]) = _EU(this,that)
    def &&(that: CtlExpr[N,T]) = And(this,that)
    def ||(that: CtlExpr[N,T]) = Or (this,that)
    def unary_!              = Not(this)
}

// Binary operators
final case class And   [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class Or    [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class _AU   [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class _EU   [N,T](left: CtlExpr[N,T], right: CtlExpr[N,T]) extends CtlExpr[N,T] 

// Unary operators
final case class AX    [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class EX    [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class AG    [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class EG    [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class AF    [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class EF    [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class Not   [N,T](op  : CtlExpr[N,T]) extends CtlExpr[N,T] 
final case class Exists[N,T]  (typeOf: TypeOf[T], op  : CtlExpr[N,T]) extends CtlExpr[N,T]

final case class Predicate[N,T]  (label: Labelizer[N,T]) extends CtlExpr[N,T]
abstract class Labelizer[N,T] {
    def test(t: N): Option[Environment[T]]
}

abstract class TypeOf[T](val varName: String) {
	def cast(n: T): Boolean
	def filter(set: Set[T]) = set.filter(cast)
}
>>>>>>> 1c6b0d311bb04df45d1f23a7ca35bedb1f88b910
