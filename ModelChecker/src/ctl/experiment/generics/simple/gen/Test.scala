package ctl.experiment.generics.simple.gen
import scala.reflect.runtime.universe._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

case class ValueString(s: String) extends Value with MetaVariable {
    override def toString = s
}

object Test extends App with Convert {
//	type GNode  = GraphNode[Node]
    type Mapping = HashMap[ValueString,MetaVarBinding[ValueString]]
    type Binding = BindingsEnv[ValueString,ValueString]
    type Env     = Environment[ValueString,ValueString]
	
	implicit def VS(s: String): ValueString = ValueString(s)
    implicit def posTuple(s: (String,String)): (ValueString,ValueString) = ValueString(s._1) -> ValueString(s._2)
    implicit def negTuple(s: (String,Set[String])): (ValueString,Set[ValueString]) =
        ValueString(s._1) -> s._2.map(ValueString(_))
	
    var i = 0
    def printMsg(failed: Boolean)     = { 
	    val msg = "\tTest %d %s".format(i,if (failed) "failed" else "passed") 
	    if (failed) Console.err.println(msg)
	    else        println(msg)
	    i += 1
	}
    def assertEquals(a0: Any, a1: Any)                      = printMsg(a0 != a1)
    def assertTrue  (b: Boolean)                            = printMsg(!b)
    def compareEnv  (envT1: Env, envT2: Env, expected: Env) = assertEquals(envT1 & envT2,expected)
    def testNeg     (env: Env, envs: Env*)                  = assertEquals(!env,Set(envs: _*))
        
    println("Testing environments...\n-----------------------")
    println("Testing compareEnv...")
    // test 0
     val env1 = new Binding ++ ("X" -> "2","Z" -> "3")
     val env2 = new Binding ++ ("Y" -> "4")
    compareEnv(env1,env2,new Binding ++ ("X" -> "2","Y" -> "4","Z" -> "3"))
    
    // test 1
    val env3 = new Binding ++ ("X" -> "7")
    val env4 = new Binding ++ ("X" -> "5")
    compareEnv(env3,env4,Bottom)
    
    // test 2
    val env5 = new Binding ++ ("X" -> "2")
    val env6 = new Binding -- ("X" -> Set("1", "2"))
    compareEnv(env5,env6,Bottom) 
    
    // test 3
    val env7 = new Binding ++ ("X" -> "15")
    val env8 = (new Binding ++ ("Y" -> "15")) -- ("X" -> Set("1", "3"))
    compareEnv(env7,env8,new Binding ++ ("X" -> "15","Y" -> "15")) 
    
    // test 4
    val env9  = new Binding -- ("X" -> Set("1", "3"))
    val env10 = new Binding -- ("X" -> Set("5", "6"))
    compareEnv(env9,env10,new Binding -- ("X" -> Set("1", "3","5","6")))
    
    println("\nTesting the negation of an environment...")
    // test 5
    testNeg(Bottom,new Binding)
    
    // test 6
    testNeg(env1,new Binding -- ("X" -> Set("2")),new Binding -- ("Z" -> Set("3")))
    
    // test 7
    testNeg(env6,new Binding ++ ("X" -> "1"),new Binding ++ ("X" -> "2"))
    
    // test 8
    testNeg(env8,new Binding ++ ("X" -> "1"),new Binding ++ ("X" -> "3"),new Binding -- ("Y" -> Set("15")))
    
    println("\nTesting '-' operation...")
    // test 9
    assertEquals((env1 & env2) - "Z",new Binding ++ ("X" -> "2","Y" -> "4"))
    
    println("\nTesting model checker...\n-----------------------")
    
//    val root  = new GNode(F(1))
//    val left  = new GNode(G(1,2))
//    val right = new GNode(G(1,3))
//    
//    root >> left
//    root >> right
//    
//    val convert = (g: GNode) => g.value match {
//        case F(x)   => Set(x)
//        case G(x,y) => Set(x,y)
//    }
//    val checker = new ModelChecker[Node,Int](root,convert)
//    
//    val env11   = Bindings[Int](Map("X" -> 2))
//    val env12   = Bindings[Int](Map("Y" -> 2))
//    val env13   = Bindings[Int](Map("Y" -> 4),Map("Y" -> Set(3)))
//    val bottom  = Bottom[Int]
//    
//    println("Testing shift...")
//    // test 10
//    val shift = checker.shift(root,Set((root,bottom),(left,env11),(root,env12),(right,bottom)),left)
//    assertEquals(shift,Set((left,bottom),(left,env12)))
    
}

abstract class Node
case class F(x: Int) extends Node
case class G(x: Int, y: Int) extends Node