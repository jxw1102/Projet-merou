package ctl

import scala.reflect.runtime.universe._
import java.util.function.Consumer


/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used in CTL 
sealed abstract class CtlExpr[M <: MetaVariable, N, V <: Value] {
    type Ctl = CtlExpr[M,N,V]
    
    def &&(that: Ctl) = And(this,that)
    def ||(that: Ctl) = Or (this,that)
    def unary_!       = Not(this)
}

// Binary operators
final case class And       [M <: MetaVariable,N,V <: Value](left   : CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V] 
final case class Or        [M <: MetaVariable,N,V <: Value](left   : CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V]
final case class AU        [M <: MetaVariable,N,V <: Value](left   : CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V]
final case class EU        [M <: MetaVariable,N,V <: Value](left   : CtlExpr[M,N,V], right: CtlExpr[M,N,V]) extends CtlExpr[M,N,V]

// Unary operators
final case class AX        [M <: MetaVariable,N,V <: Value](op     : CtlExpr[M,N,V])                        extends CtlExpr[M,N,V] 
final case class EX        [M <: MetaVariable,N,V <: Value](op     : CtlExpr[M,N,V])                        extends CtlExpr[M,N,V]
final case class Not       [M <: MetaVariable,N,V <: Value](op     : CtlExpr[M,N,V])                        extends CtlExpr[M,N,V]
final case class Exists    [M <: MetaVariable,N,V <: Value](varType: (M,TypeOf[V]),  op   : CtlExpr[M,N,V]) extends CtlExpr[M,N,V]
final case class Predicate [M <: MetaVariable,N,V <: Value](label  : Labelizer[M,N,V])                      extends CtlExpr[M,N,V]

abstract class Labelizer[M <: MetaVariable,N,V <: Value] {
	protected type Env = Environment[M,V]
    def test(n: N): Set[Env]
}

private[ctl] final class FalseLabelizer[M <: MetaVariable: TypeTag,N: TypeTag,V <: Value: TypeTag] private[ctl]() extends Labelizer[M,N,V] {
    def test(n: N): Set[Env] = Set()
}

private[ctl] final class TrueLabelizer[M <: MetaVariable: TypeTag,N: TypeTag,V <: Value: TypeTag] private[ctl]() extends Labelizer[M,N,V] {
    def test(n: N): Set[Env] = Set(new BindingsEnv)
}

object Labelizer {
    private[this] val mapTrue  = new scala.collection.mutable.HashMap[(Type,Type,Type),TrueLabelizer [_,_,_]]
    private[this] val mapFalse = new scala.collection.mutable.HashMap[(Type,Type,Type),FalseLabelizer[_,_,_]]
    
    def createTrue[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag]: TrueLabelizer[M,N,V] = 
        mapTrue.get(typeTag[M].tpe,typeTag[N].tpe,typeTag[V].tpe) match {
		    case Some(trueLabel) => trueLabel.asInstanceOf[TrueLabelizer[M,N,V]]
		    case None => 
		    	val trueLabel = new TrueLabelizer[M,N,V]
		     	mapTrue += (typeTag[M].tpe,typeTag[N].tpe,typeTag[V].tpe) -> trueLabel
		    	trueLabel
		}  
    
    def createFalse[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag]: FalseLabelizer[M,N,V] = 
        mapFalse.get(typeTag[M].tpe,typeTag[N].tpe,typeTag[V].tpe) match {
		    case Some(falseLabel) => falseLabel.asInstanceOf[FalseLabelizer[M,N,V]]
		    case None => 
		    	val falseLabel = new FalseLabelizer[M,N,V]
		     	mapFalse += (typeTag[M].tpe,typeTag[N].tpe,typeTag[V].tpe) -> falseLabel
		    	falseLabel
		}
}

class True
object True extends True

class False
object False extends False

abstract class TypeOf[V]{
    def cast(n: V): Boolean
    def filter(set: Set[V]) = set.filter(cast)
}

class NoType[V] extends TypeOf[V] {
    def cast(n: V) = true
}