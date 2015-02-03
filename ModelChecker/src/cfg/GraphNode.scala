package cfg

import scala.collection.mutable.ArrayBuffer

/**
 * This class represents an oriented graph of labelizable nodes
 * @author David Courtinot
 */
class GraphNode[U <: Labelizable[V], V <: Labelizer](val value: U) {
    private type GUV = GraphNode[U,V]
    
    private[this] val _next = ArrayBuffer[GUV]()
    private[this] val _prev = ArrayBuffer[GUV]()
    
    def prev = _prev.toList
    def next = _next.toList
    
    def addPrev(v: GUV) = _prev += v 
    def addNext(v: GUV) = _next += v 
}

/**
 * Implementation of the label fonction of a graph using the Visitor pattern
 */
trait Labelizer
trait Labelizable[V <: Labelizer] {
    def visit(visitor: V)
}