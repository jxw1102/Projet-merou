package ctl

import scala.reflect.runtime.universe._

trait MetaVariable
trait Value

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 * @author David Courtinot
 */
sealed abstract class Environment[M <: MetaVariable, V <: Value] {
	def unary_!                    : Set[Environment[M,V]] 
	def &(that: Environment[M, V]) : Environment[M,V]
	def -(variable: M)             : Environment[M,V]
}

sealed abstract class Bottom
object Bottom extends Bottom

class BottomEnv[M <: MetaVariable: TypeTag, V <: Value: TypeTag] private () extends Environment[M, V] {
	override def unary_!                    = Set(BindingsEnv(Map[M,MetaVarBinding[V]]()))
	override def &(that: Environment[M, V]) = this
	override def -(variable: M)             = this
	override def toString                   = "Bottom"
}

object BottomEnv {
	private[this] val map = new scala.collection.mutable.HashMap[(Type,Type),BottomEnv[_,_]]
  
	def create[M <: MetaVariable: TypeTag, V <: Value: TypeTag]: BottomEnv[M,V] = map.get(typeTag[M].tpe,typeTag[V].tpe) match {
	    case Some(bottom) => bottom.asInstanceOf[BottomEnv[M,V]]
	    case None => 
	    	val bottom = new BottomEnv[M,V]()
	     	map += (typeTag[M].tpe, typeTag[V].tpe) -> bottom
	    	bottom
	}  
}

trait Convert {
  implicit def botTobot[M <: MetaVariable: TypeTag, V <: Value: TypeTag](b: Bottom): BottomEnv[M,V] = 
      BottomEnv.create[M, V]
}

sealed abstract class MetaVarBinding[V <: Value]
case class PosBinding[V <: Value](value : V     ) extends MetaVarBinding[V] {
    override def equals (a: Any) = a match {
        case PosBinding(v) => v == value
        case _             => false 
    }
}
case class NegBinding[V <: Value](values: Set[V]) extends MetaVarBinding[V]{
    override def equals (a: Any) = a match {
        case NegBinding(v) => v == values
        case _             => false 
    }
}

case class BindingsEnv[M <: MetaVariable: TypeTag,V <: Value: TypeTag](bindings: Map[M, MetaVarBinding[V]] = Map[M, MetaVarBinding[V]]()) 
	extends Environment[M,V] with Convert {
    
    def this() = this(Map[M, MetaVarBinding[V]]())
    
    /**
     * This function returns the complementary of an environment, which is a set of environments
     */
	override def unary_! = {
    	bindings.flatMap { 
            case (key,PosBinding(value)) => Set(BindingsEnv(key -> NegBinding(Set(value))))
            case (key,NegBinding(value)) => value.map(v => BindingsEnv(key -> PosBinding(v)))
        }.toSet
    }
	
    /** 
     * This function return the intersection of two environment. First, it verifies the conflict
     */
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
                }}).toSeq: _*)
            case _  => that
        }
    }

	def -(variable: M) = BindingsEnv(bindings - variable)
	
	private lazy val _hashCode  = bindings.hashCode
	override def hashCode       = _hashCode
    override def equals(a: Any) = a match {
	    case BindingsEnv(b) => bindings == b
	    case _              => false
    }
	
	def ++(pos: (M,V)*)      = BindingsEnv(Map(pos.map { case (k,v) => (k,PosBinding(v)) }: _*) ++ bindings)
	def --(neg: (M,Set[V])*) = BindingsEnv(Map(neg.map { case (k,v) => (k,NegBinding(v)) }: _*) ++ bindings)
	
	override def toString = bindings.map {
	    case (k,PosBinding(v)) => "%s -> %s" .format(k,v)
	    case (k,NegBinding(v)) => "%s -/> %s".format(k,v.mkString("{ ",", "," }")) 
	}.mkString("Env(",", ",")")
}

object BindingsEnv {
    def apply[M <: MetaVariable: TypeTag, V <: Value: TypeTag](bindings: (M,MetaVarBinding[V])*): BindingsEnv[M,V] = 
        BindingsEnv(Map(bindings: _*))
}