package ctl

import scala.reflect.runtime.universe._
import java.util.NoSuchElementException


/**
 * This file contains all the definitions required to represent the environmnents used by the model-checking
 * algorithm
 * @author Zohour Abouakil
 * @author Fabien Sauce
 * @author David Courtinot
 */

/**
 * Any type that can be used as a key for a binding must extend MetaVariable
 */
trait MetaVariable

/**
 * Any type that can be used as a value for a binding must extend Value
 */
trait Value

sealed abstract class Environment[M <: MetaVariable, V <: Value] {
    /**
     * This methods returns the complementary of an environment, which is a set of environments
     */
    def unary_! : Set[Environment[M,V]] 
    
    /** 
     * This method returns the intersection of two environments
     */
    def &(that: Environment[M, V]) : Environment[M,V]

    /**
     * Creates a new Environment by removing the binding of a given key from the original environment
     */
    def -(variable: M): Environment[M, V]
    
    /**
     * Retrieve the positive binding of key m
     * @throws NoSuchElementException iff there is no positive binding associated to m
     */
    def apply(m: M): V
    
    /**
     * Retrieve the value of the positive binding associated with key m, if any
     */
    def get(m: M): Option[V]
}

/**
 * Represents the Bottom environment, which is an environment containing conflicting bindings.
 * @note for some implementation reasons, BottomEnv is not defined as a singleton, however it will
 *       behave like a singleton for a given combination of types [M,N,V].
 */
class BottomEnv[M <: MetaVariable: TypeTag, V <: Value: TypeTag] private () extends Environment[M,V] with ConvertEnv {
    override def unary_!                    = Set(Top)
    override def &(that: Environment[M, V]) = this
    override def -(variable: M)             = this
    override def apply(m: M)                = throw new NoSuchElementException
    override def get(m: M)                  = None
    override def toString                   = "Bottom"
}

object BottomEnv {
    private[this] val map = new scala.collection.mutable.HashMap[(Type,Type),BottomEnv[_,_]]
    private[ctl] def create[M <: MetaVariable: TypeTag, V <: Value: TypeTag]: BottomEnv[M,V] = 
        map.get(typeTag[M].tpe,typeTag[V].tpe) match {
            case Some(bottom) => bottom.asInstanceOf[BottomEnv[M,V]]
            case None => 
                val bottom = new BottomEnv[M,V]()
                map += (typeTag[M].tpe, typeTag[V].tpe) -> bottom
                bottom
        }  
}

sealed abstract class MetaVarBinding[V <: Value]

/**
 * Represents the value of a positive binding. 
 * @note A positive binding is a key -> value association where key is a meta-variable and value a given value
 *       that is the only value allowed for this meta-variable.
 */
case class PosBinding[V <: Value](value: V) extends MetaVarBinding[V] {
    override def equals (a: Any) = a match {
        case PosBinding(v) => v == value
        case _             => false 
    }
}

/**
 * Represents the value of a negative binding. 
 * @note A positive binding is a key -> values association where key is a meta-variable and values set of values
 *       that are not allowed for this meta-variable.
 */
case class NegBinding[V <: Value](values: Set[V]) extends MetaVarBinding[V] {
    override def equals (a: Any) = a match {
        case NegBinding(v) => v == values
        case _             => false 
    }
}

/**
 * Represents any environment containing non-conflicting positive and negative bindings.
 */
case class BindingsEnv[M <: MetaVariable: TypeTag,V <: Value: TypeTag] private[ctl] (bindings: Map[M, MetaVarBinding[V]] = Map[M, MetaVarBinding[V]]()) 
    extends Environment[M,V] with ConvertEnv {
    
    def this() = this(Map[M, MetaVarBinding[V]]())
    
    override def unary_! = {
        bindings.flatMap { 
            case (key,PosBinding(value)) => Set(BindingsEnv(key -> NegBinding(Set(value))))
            case (key,NegBinding(value)) => value.map(v => BindingsEnv(key -> PosBinding(v)))
        }.toSet
    }
    
    override def &(that: Environment[M,V]): Environment[M,V] = {
        that match {
            case BindingsEnv(b) => 
                val inter = bindings.keySet & b.keySet
                inter.foreach(key => (bindings.get(key),b.get(key)) match {
                    case (Some(NegBinding(x)),Some(PosBinding(y))) => if (x contains y) return Bottom
                    case (Some(PosBinding(x)),Some(PosBinding(y))) => if (x != y)       return Bottom
                    case (Some(PosBinding(x)),Some(NegBinding(y))) => if (y contains x) return Bottom
                    case _ =>
                })
                val dom = bindings.keySet ++ b.keySet
                BindingsEnv[M,V](dom.map(key => key -> { 
                    (bindings.get(key),b.get(key)) match {
                        case (Some(NegBinding(x)),Some(NegBinding(y))) => NegBinding(x ++ y)
                        case (Some(NegBinding(x)),Some(PosBinding(y))) => PosBinding(y)
                        case (Some(PosBinding(x)),Some(NegBinding(y))) => PosBinding(x)
                        case (_                  ,Some(x)            ) => x
                        case (Some(x)            ,_                  ) => x
                        case _ => throw new MatchError
                }}).toSeq: _*)
            case _  => that
        }
    }

    override def -(variable: M) = BindingsEnv(bindings - variable)
    override def apply(m: M)    = bindings(m) match {
        case PosBinding(x) => x
        case NegBinding(_) => throw new NoSuchElementException("No positive binding for this metavariable")
    }
    override def get(m: M)    = bindings.get(m) match {
        case Some(PosBinding(x)) => Some(x)
        case _                   => throw new NoSuchElementException("No positive binding for this metavariable")
    } 
    
    override lazy val hashCode  = bindings.hashCode
    override def equals(a: Any) = a match {
        case BindingsEnv(b) => bindings == b
        case _              => false
    }
    
    /**
     * Returns a new Environement by adding some positive bindings
     */
    def ++(pos: (M,V)*)      = BindingsEnv(Map(pos.map { case (k,v) => (k,PosBinding(v)) }: _*) ++ bindings)
    
    /**
     * Returns a new Environement by adding some negative bindings
     */
    def --(neg: (M, Set[V])*) = BindingsEnv(Map(neg.map { case (k, v) => (k, NegBinding(v)) }: _*) ++ bindings)
    
    override def toString = if (bindings.isEmpty) "Top" else bindings.map {
        case (k,PosBinding(v)) => "%s -> %s" .format(k,v)
        case (k,NegBinding(v)) => "%s -/> %s".format(k,v.mkString("{ ",", "," }")) 
    }.mkString("Env(",", ",")")
}

object BindingsEnv {
    private[this] val map = new scala.collection.mutable.HashMap[(Type,Type),BindingsEnv[_,_]]
    private[ctl] def apply[M <: MetaVariable: TypeTag, V <: Value: TypeTag](bindings: (M,MetaVarBinding[V])*): BindingsEnv[M,V] = 
        new BindingsEnv(Map(bindings: _*))
    
    private[ctl] def create[M <: MetaVariable: TypeTag, V <: Value: TypeTag]: BindingsEnv[M,V] = 
        map.get(typeTag[M].tpe,typeTag[V].tpe) match {
            case Some(top) => top.asInstanceOf[BindingsEnv[M,V]]
            case None => 
                val top = new BindingsEnv
                map += (typeTag[M].tpe, typeTag[V].tpe) -> top
                top
        } 
}

// all the following classes are used for adding some syntactic sugar for representation of Top and Bottom environments
trait ConvertEnv {
  implicit def botTobot[M <: MetaVariable: TypeTag,V <: Value: TypeTag](b: Bottom): BottomEnv  [M,V] = BottomEnv  .create[M,V]
  implicit def topTotop[M <: MetaVariable: TypeTag,V <: Value: TypeTag](b: Top   ): BindingsEnv[M,V] = BindingsEnv.create[M,V]
}

sealed abstract class Bottom
object Bottom extends Bottom

sealed abstract class Top
object Top extends Top