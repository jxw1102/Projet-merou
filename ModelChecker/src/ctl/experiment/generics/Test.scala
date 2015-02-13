package ctl.experiment.generics

object Test extends App {
    type GNode = GraphNode[Node]
    
    val root    = new GNode(F(1))
    val left    = new GNode(G(1,2))
    val right   = new GNode(G(1,3))
    
    root >> left
    root >> right
    
    val checker = new ModelChecker[Node](root)
    
}

abstract class Node
case class F(x: Int) extends Node
case class G(x: Int, y: Int) extends Node