package cfg

import collection.mutable.{ Set => MSet, HashSet => MHSet }
import ast.ProgramNode
import ctl.Environment

/**
 * This class represents an oriented unweighted graph of labelizable nodes
 * @author David Courtinot
 */
class GraphNode[U <: Labelizable[V], V <: Labelizer](val value: U) {
    private type GUV = GraphNode[U,V]
    
    private val _next = MHSet[GUV]()
    private val _prev = MHSet[GUV]()
    
    def prev = _prev
    def next = _next
    
    /**
     * 'states' contains all the states of the graph accessible from this GraphNode.
     * It should be computed on the root node.
     */
    lazy val states = getStates(this, Set())
    private def getStates(gn: GUV, set: Set[GUV]): Set[GUV] = {
        if (set contains gn) Set()
        else {
            var newSet = set + gn 
            newSet ++ _next.toSet.flatMap((g: GUV) => getStates(g,newSet) )
        }
    }
    
    /**
     * Creates a bidirectional binding from nodes this and v : this <- v
     */
    def <<(v: GUV) = { _prev += v; v._next += this; v }
    
    /**
     * Creates a bidirectional binding from nodes this and v : this -> v
     */
    def >>(v: GUV) = { _next += v; v._prev += this; v } 
    
    override def toString = value.toString
    def mkString          = addString(new StringBuilder,MSet()).toString
    
    private  def addString(sb: StringBuilder, set: MSet[GUV]): StringBuilder = {
        if (set contains this) sb 
        else if (_next.isEmpty) sb.append(this + "\n")
        else {
            set += this
            _next.foreach(node => sb.append("%s -> %s\n".format(this,node)))
            _next.filterNot(set contains _).foreach(_.addString(sb,set))
            sb
        }
    }
    
    override def equals(that: Any) = that match { case x: GUV => value == x.value case _ => false }
    override def hashCode          = value.hashCode
}

/**
 * Implementation of the label function of a graph using the Visitor pattern
 */
trait Labelizer
trait Labelizable[V <: Labelizer] { def visit(visitor: V): Option[Environment] }