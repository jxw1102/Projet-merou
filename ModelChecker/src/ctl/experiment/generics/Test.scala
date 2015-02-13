package ctl.experiment.generics

object Test extends App {
    var i = 0
    
    def printMsg(failed: Boolean)     = println("Test %d %s".format(i,if (failed) "failed" else "passed"))
    def assertEquals[T](t0: T, t1: T) = printMsg(t0 != t1)
    def assertTrue(b: Boolean)        = printMsg(!b)
    
    type GNode = GraphNode[Node]
    
    val root  = new GNode(F(1))
    val left  = new GNode(G(1,2))
    val right = new GNode(G(1,3))
    
    root >> left
    root >> right
    
    val convert = (g: GraphNode[Node]) => g.value match {
        case F(x)   => Set(x)
        case G(x,y) => Set(x,y)
    }
    val checker = new ModelChecker[Node,Int](root,convert)
    
    val env0    = Bindings[Int](Map("X" -> 2))
    val env1    = Bindings[Int](Map("Y" -> 2))
    val env2    = Bindings[Int](Map("Y" -> 4),Map("Y" -> Set(3)))
    val bottom  = Bottom[Int]
    
    // Test 0
    val shift = checker.shift(root,Set((root,bottom),(left,env0),(root,env1),(right,bottom)),left)
    assertEquals(shift,Set((left,bottom),(left,env1)))
    
    // Test 1
    assertTrue(false)
}

abstract class Node
case class F(x: Int) extends Node
case class G(x: Int, y: Int) extends Node

