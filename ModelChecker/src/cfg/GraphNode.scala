package cfg

import scala.collection.mutable.ArrayBuffer
import collection.mutable.{ Set => MSet }

/**
 * This class represents an oriented graph of labelizable nodes
 * @author David Courtinot
 */
class GraphNode[U <: Labelizable[V], V <: Labelizer](val value: U) {
    private type GUV = GraphNode[U,V]
    
    private val _next = ArrayBuffer[GUV]()
    private val _prev = ArrayBuffer[GUV]()
    
    def prev = _prev.toList
    def next = _next.toList
    
    def <<(v: GUV) = { _prev += v; v._next += this }
    def >>(v: GUV) = { _next += v; v._prev += this } 
    
    def <<<(v: Iterable[GUV]) = { _prev ++= v; v.foreach(n => n._next += this) }
    def >>>(v: Iterable[GUV]) = { _next ++= v; v.foreach(n => n._prev += this) }
    
    override def toString = value.toString
    def mkString          = addString(new StringBuilder,MSet()).toString
    
    private  def addString(sb: StringBuilder, set: MSet[GUV]): StringBuilder = {
    	_next.foreach(node => sb.append("%s -> %s\n".format(this,node)))
        if (set contains this) sb 
        else {
	        set += this
	        _next.filterNot(set contains _).foreach(_.addString(sb,set))
	        sb
        }
    }
}

/**
 * Implementation of the label fonction of a graph using the Visitor pattern
 */
trait Labelizer
trait Labelizable[V <: Labelizer] { def visit(visitor: V) }