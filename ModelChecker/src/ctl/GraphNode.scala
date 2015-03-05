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
     * State contains all the states of the graph reachable from this GraphNode.
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
     * Create a bidirectional binding from nodes this and v : this <- v
     * @return the head v of the link, in order to be able to chain the << calls
     */
    def <<(v: GNode) = { _prev += v; v._next += this; v }
    
    /**
     * Create a bidirectional binding from nodes this and v : this -> v
     * @return the head this of the link, in order to be able to chain the >> calls
     */
    def >>(v: GNode) = { _next += v; v._prev += this; v } 
    
    override def toString = value.toString
    
    def toDot             = addString(new StringBuilder,MSet())(_.toString).toString
    def toDot(name: N => String, label: N => String) = {
        def escape     (s   : String) = s.replaceAll("\"","\\\\\"")
        def formatValue(node: GNode ) = "{%s [label=\"%s\"]}".format(escape(name(node.value)),escape(label(node.value)))
        addString(new StringBuilder,MSet())(formatValue(_)).toString
    }
    
    /**
     * Search in CFG and print all arcs
     * */
    private def addString(sb: StringBuilder, set: MSet[GNode])(convert: GNode => String): StringBuilder = {
        if (set contains this) sb 
        else if (_next.isEmpty) sb.append(convert(this) + "\n")
        else {
            set += this
            _next.foreach(node => sb.append("%s -> %s\n".format(convert(this),convert(node))))
            _next.filterNot(set contains _).foreach(_.addString(sb,set)(convert))
            sb
        }
    }
}