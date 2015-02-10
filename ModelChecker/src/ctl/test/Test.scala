package ctl.test

import cfg.Labelizable
import cfg.Labelizer
import ctl.Bindings
import ctl.Environment
import ctl.ModelChecker


/**
 * @author Zohour Abouakil
 */



// Object traite les objets
object Main extends App {
//    type Pos = Map[String,Any]
//    type Neg = Map[String,Set[Any]]
    
    
    // Test environment 
        // Conflit 
       
    def compareEnv (envT1: Environment, envT2: Environment) = println(envT1 interEnv envT2)
        
    val env1 = new Bindings(Map("X" -> "2"),Map())
    val env2 = new Bindings(Map("Y" -> "4"),Map())
    
    val env3 = new Bindings(Map("X" -> "2"),Map())
    val env4 = new Bindings(Map("X" -> "2"),Map())
    
    val env5 = new Bindings(Map("X" -> "2"),Map())
    val env6 = new Bindings(Map(),Map("X" -> Set[Any]("1", "2")))
    
    val env7 = new Bindings(Map("X" -> "2"),Map())
    val env8 = new Bindings(Map("Y" -> "15"),Map("X" -> Set[Any]("1", "3")))
    
    val env9  = new Bindings(Map(),Map("X" -> Set[Any]("1", "3")))
    val env10 = new Bindings(Map(),Map("X" -> Set[Any]("5", "6")))
    
//    compareEnv(Bottom, Bottom)
//    compareEnv(new Bindings, Bottom)
//    compareEnv(env1, env2)
//    compareEnv(env3, env4)
//    compareEnv(env5, env6)
//    compareEnv(env7, env8)
//    compareEnv(env9, env10)
    
        // !
    def notEnv (envT1: Environment) = println(!envT1)
//    notEnv(Bottom)
//    notEnv(env1)
//    notEnv(env2)
//    notEnv(env3)
//    notEnv(env4)
//    notEnv(env5)
//    notEnv(env6)
//    notEnv(env7)
//    notEnv(env8)
//    notEnv(env9)
//    notEnv(env10)
    
        // -
    def minusEnv (envT1: Environment, x: String) = println(envT1-x)
//    minusEnv(Bottom, "X")
//    minusEnv(env1, "X")
//    minusEnv(env2, "X")
//    minusEnv(env3, "X")
//    minusEnv(env4, "X")
//    minusEnv(env5, "X")
//    minusEnv(env6, "X")
//    minusEnv(env7, "X")
//    minusEnv(env8, "X")
//    minusEnv(env9, "X")
//    minusEnv(env10, "X")
    
        // shift 
//    def shiftEnv (s1: GNode, T: CheckerResult, s2: GNode) = println(shiftEnv(s1, T, s2))
//    st
//    
    
    val mc = new ModelChecker[Labelizable[TestLabelizer], TestLabelizer]
    
}

abstract class Test
case class F(x: Int)         extends Test with Labelizable[TestLabelizer]
case class G(x: Int, y: Int) extends Test with Labelizable[TestLabelizer]

trait TestLabelizer extends Labelizer {
    def visitF(f: F): Option[Environment] = None
    def visitG(g: G): Option[Environment] = None
}

class FLabelizer extends TestLabelizer {
    override def visitF(f: F) = f match {
        case F(x) => Some(new Bindings(Map("X" -> x)))
    }
}

class GLabelizer extends TestLabelizer {
    override def visitG(g: G) = g match {
        case G(x, y) => Some(new Bindings(Map("X" -> x, "Y" -> y),Map()))
    }
}



//class IntNode[U, V] extends GraphNode

//type StateEnv   = (GNode, Environment)
//type GNode[U,V] = GraphNode[U,V]
//		type CheckerResult = Set[StateEnv]
//
