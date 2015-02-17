package ctl.experiment.generics

object Test extends App {
    type GNode  = GraphNode[Node]
    type Env[T] = Environment[T] 
    
    var i = 0
    
    def printMsg(failed: Boolean)     = { 
        val msg = "\tTest %d %s".format(i,if (failed) "failed" else "passed") 
        if (failed) Console.err.println(msg)
        else        println(msg)
        i += 1
    }
    def assertEquals[T](t0: T, t1: T)                                 = printMsg(t0 != t1)
    def assertTrue(b: Boolean)                                        = printMsg(!b)
    def compareEnv[T](envT1: Env[T], envT2: Env[T], expected: Env[T]) = assertEquals(envT1 interEnv envT2,expected)
    def testNeg[T](env: Env[T], envs: Env[T]*)                        = assertEquals(!env,Set(envs: _*))
    
    println("Testing environments...\n-----------------------")
    println("Testing compareEnv...")
    // test 0
    val env1 = Pos("X" -> "2", "Z" -> "3")
    val env2 = Pos("Y" -> "4")
    compareEnv(env1,env2,Pos("X" -> "2","Y" -> "4","Z" -> "3"))
    
    // test 1
    val env3 = Pos("X" -> "2")
    val env4 = Pos("X" -> "2")
    compareEnv(env3,env4,Bottom[String]())
    
    // test 2
    val env5 = Pos("X" -> "2")
    val env6 = Neg("X" -> Set("1", "2"))
    compareEnv(env5,env6,Bottom[String]()) 
    
    // test 3
    val env7 = Pos("X" -> "2")
    val env8 = Bindings(Map("Y" -> "15"),Map("X" -> Set("1", "3")))
    compareEnv(env7,env8,Pos("X" -> "2","Y" -> "15")) 
    
    // test 4
    val env9  = Neg("X" -> Set("1", "3"))
    val env10 = Neg("X" -> Set("5", "6"))
    compareEnv(env9,env10,Neg("X" -> Set("1", "3","5","6")))
    
    println("\nTesting the negation of an environment...")
    // test 5
    testNeg(Bottom[String](),Bindings[String]())
    
    // test 6
    testNeg(env1,Neg("X" -> Set("2")),Neg("Z" -> Set("3")))
    
    // test 7
    testNeg(env6,Pos("X" -> "1"),Pos("X" -> "2"))
    
    // test 8
    testNeg(env8,Pos("X" -> "1"),Pos("X" -> "3"),Neg("Y" -> Set("15")))
    
    println("\nTesting '-' operation...")
    // test 9
    assertEquals(env1.interEnv(env2) - "Z",Pos("X" -> "2","Y" -> "4"))
    
    println("\nTesting model checker...\n-----------------------")
    
    val root  = new GNode(F(1))
    val left  = new GNode(G(1,2))
    val right = new GNode(G(1,3))
    
    root >> left
    root >> right
    
    val convert = (g: GNode) => g.value match {
        case F(x)   => Set(x)
        case G(x,y) => Set(x,y)
    }
    val checker = new ModelChecker[Node,Int](root,convert)
    
    val env11   = Bindings[Int](Map("X" -> 2))
    val env12   = Bindings[Int](Map("Y" -> 2))
    val env13   = Bindings[Int](Map("Y" -> 4),Map("Y" -> Set(3)))
    val bottom  = Bottom[Int]
    
    println("Testing shift...")
    // test 10
    val shift = checker.shift(root,Set((root,bottom),(left,env11),(root,env12),(right,bottom)),left)
    assertEquals(shift,Set((left,bottom),(left,env12)))
}

abstract class Node
case class F(x: Int) extends Node
case class G(x: Int, y: Int) extends Node

