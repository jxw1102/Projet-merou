package ctl.experiment.generics

import ast.ProgramNodeLabelizer

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used in CTL 
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