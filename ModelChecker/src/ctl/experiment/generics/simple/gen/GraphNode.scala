package ctl.experiment.generics.simple.gen

import collection.mutable.{ Set => MSet, HashSet => MHSet }
import ast.ProgramNode

/**
 * This class represents an oriented unweighted graph of labelizable nodes
 * @author David Courtinot
 */
class GraphNode[N](val value: N) {
    private type GNode = GraphNode[N]
    
    private val _next = MHSet[GNode]()
    private val _prev = MHSet[GNode]()
    
    def prev = _prev
    def next = _next
    
    /**
     * 'states' contains all the states of the graph accessible from this GraphNode.
     * It should be computed on the root node.
     */
    lazy val states = getStates(Set())
    private def getStates(set: Set[GNode]): Set[GNode] = {
        if (set contains this) Set()
        else {
            var newSet = set + this 
            newSet ++ _next.toSet.flatMap((g: GNode) => g.getStates(newSet) )
        }
    }
    
    /**
     * Creates a bidirectional binding from nodes this and v : this <- v
     */
    def <<(v: GNode) = { _prev += v; v._next += this; v }
    
    /**
     * Creates a bidirectional binding from nodes this and v : this -> v
     */
    def >>(v: GNode) = { _next += v; v._prev += this; v } 
    
    override def toString = value.toString
    def mkString          = addString(new StringBuilder,MSet()).toString
    
    private  def addString(sb: StringBuilder, set: MSet[GNode]): StringBuilder = {
        if (set contains this) sb 
        else if (_next.isEmpty) sb.append(this + "\n")
        else {
            set += this
            _next.foreach(node => sb.append("%s -> %s\n".format(this,node)))
            _next.filterNot(set contains _).foreach(_.addString(sb,set))
            sb
        }
    }
    
    override def equals(that: Any) = that match { case x: GNode => value == x.value case _ => false }
    override def hashCode          = value.hashCode
}