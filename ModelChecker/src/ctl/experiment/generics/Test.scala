package ctl.experiment.generics

object Test extends App {
    
    def compareEnv (envT1: Environment[String], envT2: Environment[String]) = println("compareEnv " + (envT1 interEnv envT2))
        
    val env1 = new Bindings(Map("X" -> "2", "Z" -> "3"),Map())
    val env2 = new Bindings(Map("Y" -> "4"),Map())
    compareEnv(env1,env2)    // correct
    
    val env3 = new Bindings(Map("X" -> "2"),Map())
    val env4 = new Bindings(Map("X" -> "2"),Map())
    compareEnv(env3,env4)    // correct
    
    val env5 = new Bindings(Map("X" -> "2"),Map())
    val env6 = new Bindings(Map(),Map("X" -> Set[String]("1", "2")))
    compareEnv(env5,env6)    // correct
    
    val env7 = new Bindings(Map("X" -> "2"),Map())
    val env8 = new Bindings(Map("Y" -> "15"),Map("X" -> Set[String]("1", "3")))
    compareEnv(env7,env8)    // correct
    
    val env9  = new Bindings(Map(),Map("X" -> Set[String]("1", "3")))
    val env10 = new Bindings(Map(),Map("X" -> Set[String]("5", "6")))
    compareEnv(env9,env10)   // correct
    
    
    def notEnv (envT1: Environment[String]) = println("notEnv " + !envT1)
    notEnv(Bottom())    // correct
    notEnv(env1)        // correct
    notEnv(env6)        // correct
    notEnv(env8)        // correct
    
    
    def minusEnv (envT1: Environment[String], x: String) = println(envT1-x)
    minusEnv(env1 interEnv env2, "Z")    // correct
    
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