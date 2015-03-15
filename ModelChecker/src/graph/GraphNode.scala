/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 *	Author(s) :
 *    - David Courtinot
 *    - Xiaowen Ji
 */

package graph

import collection.mutable.{Set => MSet, HashSet => MHSet}

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
    
    def toDot = addString(new StringBuilder,MSet())(_.toString).toString
    def toDot(name: N => String, label: N => String) = {
        def escape     (s   : String) = s.replaceAll("\"","\\\\\"")
        def formatValue(node: GNode ) = "{%s [label=\"%s\"]}".format(escape(name(node.value)),escape(label(node.value)))
        addString(new StringBuilder,MSet())(formatValue(_)).toString
    }
    
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