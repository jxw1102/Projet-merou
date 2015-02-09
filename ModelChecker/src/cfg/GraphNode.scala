package cfg

import scala.collection.mutable.ArrayBuffer

/**
 * This class represents an oriented graph of labelizable nodes
 * @author David Courtinot
 */
class GraphNode[U <: Labelizable[V], V <: Labelizer](val value: U) {
    private type GUV = GraphNode[U,V]
    
    private val _next = ArrayBuffer[GUV]()
    private val _prev = ArrayBuffer[GUV]()
    
    lazy val states = getStates(this, Set())
    
    private def getStates(gn: GUV, set: Set[GUV]): Set[GUV] = {
        if (set contains gn) 
            Set()
        else {
            var newSet = set + gn 
            newSet ++ _next.toSet.flatMap((g: GUV) => getStates(g,newSet) )
        }
    }
    
    
    
    def prev = _prev.toList
    def next = _next.toList
    
    def <<(v: GUV) = { _prev += v; v._next += this }
    def >>(v: GUV) = { _next += v; v._prev += this } 
    
    def <<<(v: Iterable[GUV]) = { _prev ++= v; v.foreach(n => n._next += this) }
    def >>>(v: Iterable[GUV]) = { _next ++= v; v.foreach(n => n._prev += this) }
}

/**
 * Implementation of the label fonction of a graph using the Visitor pattern
 */
trait Labelizer
trait Labelizable[V <: Labelizer] { def visit(visitor: V) }