package ctl.experiment.generics.simple.gen
import scala.reflect.runtime.universe._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

object Test extends App with Convert {
    type Mapping = HashMap[Identifier,MetaVarBinding[Identifier]]
    type Binding = BindingsEnv[Identifier,Identifier]
    type Env     = Environment[Identifier,Identifier]
    
	
    implicit def strToId(s: String): Identifier = Identifier(s)
    implicit def posTuple(s: (String,String)): (Identifier,Identifier) = Identifier(s._1) -> Identifier(s._2)
    implicit def negTuple(s: (String,Set[String])): (Identifier,Set[Identifier]) =
        Identifier(s._1) -> s._2.map(Identifier(_))
	
    var i = 0
    def printMsg(failed: Boolean)     = { 
	    val msg = "\tTest %d %s".format(i,if (failed) "failed" else "passed") 
	    if (failed) Console.err.println(msg)
	    else        println(msg)
	    i += 1
	}
    def assertEquals[T](a0: T, a1: T)                       = { /*println((a0,a1));*/ printMsg(a0 != a1) }
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
    type GNode    = GraphNode[Node]
    type Binding2 = BindingsEnv[Identifier, IntVal]
    type Env2     = Environment[Identifier, IntVal]
    type StateEnv = (GNode, Environment[Identifier, IntVal])
    
    implicit def intToVal(n: Int)  : IntVal     = IntVal(n)
    implicit def posTupleId(s: (String,Int)): (Identifier,IntVal) = Identifier(s._1) -> IntVal(s._2)
    implicit def negTupleId(s: (String,Set[Int])): (Identifier,Set[IntVal]) =
        Identifier(s._1) -> s._2.map(IntVal(_))
    
    val root  = new GNode(F(1))
    val left  = new GNode(G(1,2))
    val right = new GNode(G(1,3))
    
    root >> left
    root >> right
    
    val convert = (g: GNode) => g.value match {
        case F(x)   => Set(IntVal(x))
        case G(x,y) => Set(IntVal(x),IntVal(y))
    }
    val checker     = new ModelChecker[Identifier,Node,IntVal](root,convert)
    val env11       = new Binding2 ++ ("X" -> 7)
    val env12       = (new Binding2 ++ ("Y" -> 6)) -- ("X" -> Set(3,5))
    
    println("Testing shift...")
    // test 10
    val shift = checker.shift(root,Set((root,Bottom),(left,env11),(root,env12),(right,Bottom)),left)
    assertEquals(shift,Set[(GNode,Env2)]((left,Bottom),(left,env12)))
    
    
    println("\nTesting interStateEnv...")
    
   
    def testInterStateEnv(se1 : StateEnv, se2 : StateEnv, expected: Option[StateEnv]) {
        val interStateEnv = checker.interStateEnv(se1, se2)
        assertEquals(interStateEnv,expected)
    }
    
    // test 11
    val sEnv11 = (root, env11) 
    val sEnv12 = (root, env12) 
    
    val env13  = new Binding2 ++ ("X" -> 4)
    val env14  = (new Binding2 ++ ("Y" -> 6)) -- ("X" -> Set(3,5))
    
    val sEnv13 = (root, env13) 
    val sEnv14 = (root, env14) 
    val sEnv15 = (root, (new Binding2 ++  ("Y" -> 6)) ++("X" -> 4))
     
    testInterStateEnv(sEnv13, sEnv14, Some(sEnv15))
    
    // test 12
    val env16  = new Binding2 ++ ("X" -> 4) ++ ("Y" -> 6) -- ("Z" -> Set(3,5))
    val env17  = (new Binding2 ++ ("Y" -> 6)) -- ("X" -> Set(3,5)) -- ("Z" -> Set(7,9))
    val sEnv16 = (root, env16) 
    val sEnv17 = (root, env17) 
    val sEnv18 = (root, new Binding2 ++  ("Y" -> 6, "X" -> 4) -- ("Z" -> Set(3,5,7,9)))
     
    testInterStateEnv(sEnv16, sEnv17, Some(sEnv18))
    
    // test 13
    val env19  = new Binding2 ++ ("X" -> 4, "Y" -> 6) -- ("Z" -> Set(3,5))
    val env20  = (new Binding2 ++ ("Y" -> 6)) -- ("X" -> Set(3,5), "Z" -> Set(7,9))
    val sEnv19 = (left, env19) 
    val sEnv20 = (root, env20) 
    val sEnv21 = None
     
    testInterStateEnv(sEnv19, sEnv20, sEnv21)
    
    println("\nTesting existsone...")
    // test 14
    assertEquals(checker.existsone("X", sEnv19), (left, new Binding2 ++ ("Y" -> 6) -- ("Z" -> Set(3,5))))
    
    // test 15 
    assertEquals(checker.existsone("Z", sEnv19), (left, new Binding2 ++ ("X" -> 4, "Y" -> 6)))
    
    println("\nTesting negone...")
    // test 16   
    val notX     = new Binding2 -- ("X" -> Set(4))
    val notY     = new Binding2 -- ("Y" -> Set(6))
    val notZ1    = new Binding2 ++ ("Z" ->     3 )
    val notZ2    = new Binding2 ++ ("Z" ->     5 )
    val expected = Set((root,notX),(root,notY),(root,notZ1),(root,notZ2)) ++ Set((left,new Binding2),(right,new Binding2))
    assertEquals(checker.negone((root,env19)),expected)
    
    println("\nTesting disj...")
    // test 17
    val set1: Set[StateEnv] = Set((root,notX),(root,notY),(left,notZ1),(right,notZ2))
    val set2: Set[StateEnv] = Set((left,new Binding2),(right,new Binding2), (left,notZ1))
    assertEquals(checker.disj(set1, set2), Set((root,notX),(root,notY),(left,notZ1),(right,notZ2), (left,new Binding2),(right,new Binding2)))
    
    println("\nTesting Disj...")
    // test 18
    val set3: Set[StateEnv] = Set(sEnv19,sEnv20)
    assertEquals(checker.Disj(Set(set1, set2, set3)), 
            Set((root,notX),(root,notY), (left,notZ1),(right,notZ2), (left,new Binding2),(right,new Binding2), sEnv19,sEnv20))
            
    println("\nTesting conj...")
    // test 19
    assertEquals(checker.conj(set1, set2), Set((left,notZ1),(right,notZ2)))      
    // test 20
    assertEquals(checker.conj(set1, set3), Set((root, (new Binding2 ++ ("Y" -> 6)) -- ("X" -> Set(3,5,4), "Z" -> Set(7,9)))))
    
    println("\nTesting conjFold...")
    // test 21
    assertEquals(checker.conjFold(Set(set1, set2)), Set((left,notZ1),(right,notZ2)))
    // test 22
    assertEquals(checker.conjFold(Set(set1, set3)), Set((root, (new Binding2 ++ ("Y" -> 6)) -- ("X" -> Set(3,5,4), "Z" -> Set(7,9)))))
    // test 23
    assertEquals(checker.conjFold(Set(set1, set2, set3)), Set())
    // test 24
    val set4 : Set[StateEnv] = Set((root, (new Binding2 ++ ("Z" -> 8)) -- ("X" -> Set(3,5,4), "Z" -> Set(7,9))))
    assertEquals(checker.conjFold(Set(set1, set4, set3)), Set((root, (new Binding2 ++ ("Y" -> 6)) ++ ("Z" -> 8) -- ("X" -> Set(3,5,4)))))
                 
}

abstract class Node
case class F(x: Int)         extends Node
case class G(x: Int, y: Int) extends Node

case class Identifier(name: String) extends Value with MetaVariable {
    override def hashCode       = name.hashCode
    override def equals(a: Any) = a match {
        case Identifier(value) => value == name
        case _                 => false 
    }
    override def toString       = name
}

case class IntVal(n: Int) extends Value