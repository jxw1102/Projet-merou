package ctl

import cfg.GraphNode
import ast.ProgramNodeLabelizer
import ast.ProgramNode

/**
 * @author Zohour Abouakil
 */
object ModelChecker {
    type StateEnv         = (GraphNodeProgram, Environment)
    type GraphNodeProgram = GraphNode[ProgramNode, ProgramNodeLabelizer]
    type CheckerResult    = Set[StateEnv]
    
    // j'ai remplacé les * par des / car quand on génèrera la scaladoc les /** ... */ seront interprétés
    // comme des docstring
    
    //////////////////////////---------------------------------------------//////////////////////////
    ///////////////////////////// Function implementation only for CTL-V //////////////////////////// 
    //////////////////////////---------------------------------------------//////////////////////////
    def shift(s1: GraphNodeProgram , T: CheckerResult, s2: GraphNodeProgram): CheckerResult = {
        return T.filter { case(a,b) => a == s1 }.map{ case(a,b) => (s2,b) } // return a new set 
    }
    
    def interStateEnv(se1: StateEnv, se2: StateEnv): Option[StateEnv] = {
        if (se1._1 != se2._1) None
        else                  Some((se1._1, se1._2 interEnv se2._2)) 
    }
    
    /**
     * The function existsone discards the binding of a quantified variable x from the environment of a state/environment pair
     */
    def existsone(metaData: String, ev: StateEnv) : StateEnv = (ev._1, ev._2 - metaData)
    
    /**
     * The function inj, used to inject the result of matching a predicate into the codomain of SAT
     */
    def inj(s: GraphNodeProgram, env: Environment): StateEnv = (s, env)
    def same(t1: CheckerResult , t2: CheckerResult)          = t1 == t2
    
    val nodeParent = new GraphNodeProgram(???) // to modify 
    def negone(s: GraphNodeProgram, env: Environment) = {
        ((!env).map { case value => (s, value) } 
        ++ 
        nodeParent.states.map { node => if (node != s) inj(node, new Bindings) })      
    }

    // non, dans le cas général on ne sait rien sur Val. On a calculé l'attribut states sur GraphNode
    // pour récupérer l'ensemble des états
    def ex_binding(metaData: String, se: StateEnv) = true // On considere que l ensemble Val est infini
    
    //////////////////////////---------------------------------------------///////////////////////////
    //////////////////////////////////////// common functions //////////////////////////////////////// 
    //////////////////////////---------------------------------------------///////////////////////////
    def disj(t1: CheckerResult , t2: CheckerResult) = t1 ++ t2
    
    def Disj = {}
    
    def Conj = {}
    
    def conj(T1: CheckerResult , T2: CheckerResult) = 
        for (t1 <- T1 ; t2 <- T2 ; inter = interStateEnv(t1,t2) ; if (inter.isDefined)) yield inter 
        
    def neg(T: CheckerResult) = {}
    
    def preA(T: CheckerResult) = {}
    
    def preE(T: CheckerResult) = {}
    
    def SAT_AU(T1: CheckerResult , T2: CheckerResult) = {}

    def SAT_EU(T1: CheckerResult , T2: CheckerResult) = {}
    
    def exits(x: String, T: CheckerResult) = for (t <- T ; if (ex_binding(x, t))) yield existsone(x, t)
}