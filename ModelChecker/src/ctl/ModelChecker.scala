package ctl

import cfg.GraphNode
import ast.ProgramNodeLabelizer
import ast.ProgramNode

/**
 * @author simple
 */

object ModelChecker {
    type StateEnv         = (GraphNodeProgram, Environment)
    type GraphNodeProgram = GraphNode[ProgramNode, ProgramNodeLabelizer]
    type CheckerResult    = Set[StateEnv]
    
    
    def shift(s1: GraphNodeProgram , T: CheckerResult, s2: GraphNodeProgram): CheckerResult = {
        return T.filter{case(a,b) => a == s1}.map{case(a,b) => (s2,b)}          // return a new set 
    }
    
    
    def interStateEnv(se1: StateEnv, se2: StateEnv): Option[StateEnv] = {
        if(se1._1 != se2._1) 
            return None
            
        val interBothEnv = se1._2 interEnv se2._2
        
        interBothEnv match {
            case Some(value) => return Some((se1._1, value))
            case None        => return None
        }
    }
    
    
    def existsone(metaData: String, ev: StateEnv): StateEnv = {
        return (ev._1, ev._2 - metaData)
    }
        
    
    def inj(s: GraphNodeProgram, env: Environment): StateEnv = {
        return (s, env)
    }
    
}