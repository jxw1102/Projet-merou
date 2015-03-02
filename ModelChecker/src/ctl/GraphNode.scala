package ctl

import collection.mutable.{Set => MSet, HashSet => MHSet}
import scala.collection.mutable.{HashSet => MHSet}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{HashSet => MHSet}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{HashSet => MHSet}
import scala.collection.mutable.{Set => MSet}

/**
 * This class represents an oriented unweighted graph
 * @author David Courtinot
 * @author Xiaowen Ji
 */
class GraphNode[N](val value: N) {
    private type GNode = GraphNode[N]
    
    private val _next = MHSet[GNode]()
    private val _prev = MHSet[GNode]()
    
    def prev = _prev
    def next = _next
    
    /**
     * states contains all the states of the graph reachable from this GraphNode.
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
     * @return the head v of the link, in order to be able to chain the << calls
     */
    def <<(v: GNode) = { _prev += v; v._next += this; v }
    
    /**
     * Creates a bidirectional binding from nodes this and v : this -> v
     * @return the head this of the link, in order to be able to chain the >> calls
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
    
    def mkString(name: N => String, label: N => String) = addString(name,label,new StringBuilder,MSet()).toString

    private  def addString(name: N => String, label: N => String, sb: StringBuilder, set: MSet[GNode]): StringBuilder = {
        def formatValue(node: GNode) = "{%s [label=\"%s\"]}".format(name(node.value),label(node.value)).replaceAll("\"","''")
        
        if (set contains this) sb
        else if (_next.isEmpty) sb.append(formatValue(this) + "\n")
        else {
            set += this
            _next.foreach(node => sb.append("%s -> %s\n".format(formatValue(this),formatValue(node))))
            _next.filterNot(set contains _).foreach(_.addString(sb,set))
            sb
        }
    }
}