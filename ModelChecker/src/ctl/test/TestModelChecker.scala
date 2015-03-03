package ctl.test

import scala.annotation.migration
import scala.reflect.runtime.universe

import ctl._

/**
 * This class contains automated tests for the ModelChecker. More specifically, it includes unitary tests
 * for some elementary methods of the ModelChecker as well as more advanced tests which consists in evaluating
 * some CTL expressions on basic graphs.
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 * @author David Courtinot
 */
object TestModelChecker extends App with TestUtils with ConvertEnv {
    type GNode    = GraphNode[Node]
    type Binding  = BindingsEnv[Identifier, IntVal]
    type Env      = Environment[Identifier, IntVal]
    type StateEnv = (GNode, Environment[Identifier, IntVal])
    
    implicit def intToVal  (n: Int)              : IntVal                   = IntVal(n)
    implicit def posTupleId(s: (String,Int))     : (Identifier,IntVal)      = Identifier(s._1) -> IntVal(s._2)
    implicit def negTupleId(s: (String,Set[Int])): (Identifier,Set[IntVal]) = Identifier(s._1) -> s._2.map(IntVal(_))
    
    newTestSession
    println("Unitary tests...\n-----------------------")
    unitaryTests
    
    newTestSession
    println("\nAdvanced tests...\n-----------------------")
    advancedTests

    def unitaryTests = {
        val root  = new GNode(F(1))
        val left  = new GNode(G(2))
        val right = new GNode(G(2))
        		
        root >> left
        root >> right
        		
        val checker = new ModelChecker[Identifier,Node,IntVal](root,Node.convert)
        
        def testInterStateEnv(se1 : StateEnv, se2 : StateEnv, expected: Option[StateEnv]) {
        	val interStateEnv = checker.interStateEnv(se1, se2)
        	assertEquals(interStateEnv,expected)
        }
        
        val env11 = new Binding ++ ("X" -> 7)
        val env12 = (new Binding ++ ("Y" -> 6)) -- ("X" -> Set(3,5))
        				
        println("\nTesting model checker...\n-----------------------")
        println("Testing shift...")
        // test 0
        val shift = checker.shift(root,Set((root,Bottom),(left,env11),(root,env12),(right,Bottom)),left)
        assertEquals(shift,Set[(GNode,Env)]((left,Bottom),(left,env12)))
        				
        println("\nTesting interStateEnv...")
        // test 1
        val sEnv11 = (root, env11) 
        val sEnv12 = (root, env12) 
        				
        val env13  = new Binding ++ ("X" -> 4)
        val env14  = (new Binding ++ ("Y" -> 6)) -- ("X" -> Set(3,5))
        
        val sEnv13 = (root, env13) 
        val sEnv14 = (root, env14) 
        val sEnv15 = (root, (new Binding ++  ("Y" -> 6)) ++("X" -> 4))
        testInterStateEnv(sEnv13, sEnv14, Some(sEnv15))
        				
        // test 2
        val env16  = new Binding ++ ("X" -> 4) ++ ("Y" -> 6) -- ("Z" -> Set(3,5))
        val env17  = (new Binding ++ ("Y" -> 6)) -- ("X" -> Set(3,5)) -- ("Z" -> Set(7,9))
        val sEnv16 = (root, env16) 
        val sEnv17 = (root, env17) 
        val sEnv18 = (root, new Binding ++  ("Y" -> 6, "X" -> 4) -- ("Z" -> Set(3,5,7,9)))
        testInterStateEnv(sEnv16, sEnv17, Some(sEnv18))
        				
        // test 3
        val env19  = new Binding ++ ("X" -> 4, "Y" -> 6) -- ("Z" -> Set(3,5))
        val env20  = (new Binding ++ ("Y" -> 6)) -- ("X" -> Set(3,5), "Z" -> Set(7,9))
        val sEnv19 = (left, env19) 
        val sEnv20 = (root, env20) 
        val sEnv21 = None
        testInterStateEnv(sEnv19, sEnv20, sEnv21)
        
        println("\nTesting existsone...")
        // test 4
        assertEquals(checker.existsone("X", sEnv19), (left, new Binding ++ ("Y" -> 6) -- ("Z" -> Set(3,5))))
        
        // test 5 
        assertEquals(checker.existsone("Z", sEnv19), (left, new Binding ++ ("X" -> 4, "Y" -> 6)))
        
        println("\nTesting negone...")
        // test 6   
        val notX     = new Binding -- ("X" -> Set(4))
        val notY     = new Binding -- ("Y" -> Set(6))
        val notZ1    = new Binding ++ ("Z" ->     3 )
        val notZ2    = new Binding ++ ("Z" ->     5 )
        val expected = Set((root,notX),(root,notY),(root,notZ1),(root,notZ2)) ++ Set((left,new Binding ),(right,new Binding))
        assertEquals(checker.negone((root,env19)),expected)
        				
        println("\nTesting disj...")
        // test 7
        val set1: Set[StateEnv] = Set((root,notX),(root,notY),(left,notZ1),(right,notZ2))
        val set2: Set[StateEnv] = Set((left,new Binding),(right,new Binding), (left,notZ1))
        assertEquals(checker.disj(set1, set2), Set((root,notX),(root,notY),(left,notZ1),(right,notZ2), (left,new Binding),(right,new Binding)))
        				
        println("\nTesting Disj...")
        // test 8
        val set3: Set[StateEnv] = Set(sEnv19,sEnv20)
        assertEquals(checker.Disj(Set(set1, set2, set3)), 
        		Set((root,notX),(root,notY), (left,notZ1),(right,notZ2), (left,new Binding),(right,new Binding), sEnv19,sEnv20))
        						
        println("\nTesting conj...")
        // test 9
        assertEquals(checker.conj(set1, set2), Set((left,notZ1),(right,notZ2)))      
        // test 10
        assertEquals(checker.conj(set1, set3), Set((root, (new Binding ++ ("Y" -> 6)) -- ("X" -> Set(3,5,4), "Z" -> Set(7,9)))))
        
        println("\nTesting conjFold...")
        // test 11
        assertEquals(checker.conjFold(Set(set1, set2)), Set((left,notZ1),(right,notZ2)))
        // test 12
        assertEquals(checker.conjFold(Set(set1, set3)), Set((root, (new Binding ++ ("Y" -> 6)) -- ("X" -> Set(3,5,4), "Z" -> Set(7,9)))))
        // test 13
        assertEquals(checker.conjFold(Set(set1, set2, set3)), Set())
        // test 14
        val set4 : Set[StateEnv] = Set((root, (new Binding ++ ("Z" -> 8)) -- ("X" -> Set(3,5,4), "Z" -> Set(7,9))))
        assertEquals(checker.conjFold(Set(set1, set4, set3)), Set((root, (new Binding ++ ("Y" -> 6)) ++ ("Z" -> 8) -- ("X" -> Set(3,5,4)))))
    
        println("\nTesting neg...")
        // test 15
        println(checker.conjFold(Set()) + " ****")
    
        println("\nTesting preA...")
        // test 15
        val set5: Set[StateEnv] = Set((left, new Binding ++ ("X" -> 2)), (right, new Binding ++ ("X" -> 7)))
        println("\nTesting preE...")
        println(checker.preA(set5))        
        println(checker.preE(set5))
        
    }
        
    def advancedTests = {
        implicit def noType(s: String) = (Identifier(s), new NoType[IntVal])
        
        type MC                 = ModelChecker[Identifier,Node,IntVal]
        val (rootA,rootB,rootC) = (example2a,example2b,example2c)
        val (mcA,mcB,mcC)       = (new MC(rootA,Node.convert),new MC(rootB,Node.convert),new MC(rootC,Node.convert))
        
        def f(s: String)             = Predicate(NodeLabelizer("f",s))
        def g(s: String)             = Predicate(NodeLabelizer("g",s))
        def h(s0: String,s1: String) = Predicate(NodeLabelizer("h",s0,s1))
        
        // test 0
        assertEquals(mcA.evalExpr(f("y")),Set((rootA,new Binding ++ ("y" -> 1))))
        // test 1
        assertEquals(mcA.evalExpr(f("x") && AX(g("y") && AX(h("x","y")))),Set((rootA,new Binding ++ ("x" -> 1,"y" -> 2))))
        // test 2
        assertEquals(mcA.evalExpr(f("x") && AX(Exists("y",g("y") && AX(h("x","y"))))),Set((rootA,new Binding ++ ("x" -> 1))))
        // test 3
        assertEquals(mcB.evalExpr(f("x") && AX(Exists("y",g("y") && AX(h("x","y"))))),Set((rootB,new Binding ++ ("x" -> 1))))
        // test 4
        assertEquals(mcC.evalExpr(f("x") && AX(Exists("y",g("y") && AX(h("x","y"))))),Set())
        // test 5
        println(mcA.evalExpr(AU(f("x"), (g("x")))))
    }
    
    // example of the figure 2.a in popl.pdf
    def example2a = {
        val root   = new GNode(F(1))
        val left1  = new GNode(G(2))
        val left2  = new GNode(H(1,2))
        val right1 = new GNode(G(2))
        val right2 = new GNode(H(1,2))
        		
        root >> left1  >> left2  >> left2
        root >> right1 >> right2 >> right2
        
        root
    }
    
    // example of the figure 2.b in popl.pdf
    def example2b = {
        val root   = new GNode(F(1))
        val left1  = new GNode(G(2))
        val left2  = new GNode(H(1,2))
        val right1 = new GNode(G(3))
        val right2 = new GNode(H(1,3))
        		
        root >> left1  >> left2  >> left2
        root >> right1 >> right2 >> right2
        
        root
    }
    
    // example of the figure 2.b in popl.pdf
    def example2c = {
        val root   = new GNode(F(1))
        val left1  = new GNode(G(2))
        val left2  = new GNode(H(1,3))
        val right1 = new GNode(G(3))
        val right2 = new GNode(H(1,3))
        		
        root >> left1  >> left2  >> left2
        root >> right1 >> right2 >> right2
        
        root
    }
    
}

abstract class Node(val id: Int) {
    override def equals(a: Any) = a match {
        case x: Node => x.id == id
        case _       => false
    }
    
    override def hashCode = id
}
object Node {
    private var id = 0
    
    def getId = { val res = id; id += 1; res }
    
    def convert = (node: Node) => node match {
    	case F(x)   => Set(IntVal(x))
        case G(x)   => Set(IntVal(x))
        case H(x,y) => Set(IntVal(x),IntVal(y))
    }
}
case class F(x: Int)         extends Node(Node.getId)
case class G(x: Int)         extends Node(Node.getId)
case class H(x: Int, y: Int) extends Node(Node.getId)

case class NodeLabelizer(op: String, metavars: Identifier*) extends Labelizer[Identifier,Node,IntVal] {
    def test(n: Node) = (n,op.toLowerCase) match {
        case (F(x)  ,"f") => Set(new BindingsEnv ++ (metavars(0) -> IntVal(x)))
        case (G(x)  ,"g") => Set(new BindingsEnv ++ (metavars(0) -> IntVal(x)))
        case (H(x,y),"h") => Set(new BindingsEnv ++ (metavars(0) -> IntVal(x),metavars(1) -> IntVal(y)))
        case _            => Set()
    }
}

case class IntVal(n: Int) extends Value {
    override def toString = n.toString
}