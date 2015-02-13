package ctl.experiment.generics

import ast.ProgramNodeLabelizer
import ast.ProgramNode

/**
 * @author Zohour Abouakil
 */
class ModelChecker[N,T](val root: GraphNode[N], convert: GraphNode[N] => Set[T]) {
    type StateEnv      = (GNode, Environment[T])
    type GNode         = GraphNode[N]
    type CheckerResult = Set[StateEnv]
    
    lazy val Val: Set[T] = root.states.flatMap(convert).toSet
    
    def evalExpr(expr : CtlExpr[N,T]): CheckerResult = expr match {
	        case And   (x, y)    => conj  (evalExpr(x),evalExpr(y))
	        case Or    (x, y)    => disj  (evalExpr(x),evalExpr(y))
	        case _AU   (x, y)    => SAT_AU(evalExpr(x),evalExpr(y))
	        case _EU   (x, y)    => SAT_EU(evalExpr(x),evalExpr(y))
	        case AX    (x   )    => preA  (evalExpr(x))
	        case EX    (x   )    => preE  (evalExpr(x))
	        case Not   (x   )    => neg   (evalExpr(x))
            case Exists (x, y)   => exists(x,evalExpr(y))
	        case Predicate(x) => 
                for (n <- root.states ; env = x.test(n.value) ; if(env.isDefined)) yield (n,env.get)           
    }
    
    //////////////////////////---------------------------------------------//////////////////////////
    ///////////////////////////// Function implementation only for CTL-V //////////////////////////// 
    //////////////////////////---------------------------------------------//////////////////////////
    def shift(s1: GNode , T: CheckerResult, s2: GNode): CheckerResult = T.filter { case(a,b) => a == s1 }.map{ case(a,b) => (s2,b) }
    
    def interStateEnv(se1: StateEnv, se2: StateEnv): Option[StateEnv] = {
        if (se1._1 != se2._1) None
        else                  Some((se1._1, se1._2 interEnv se2._2)) 
    }
    
    /**
     * The existsone method discards the binding of a quantified variable x from the Environment of a state/Environment pair
     */
    private def existsone(metaData: String, ev: StateEnv) : StateEnv = (ev._1, ev._2 - metaData)
    
    /**
     * The function inj, used to inject the result of matching a predicate into the codomain of SAT
     */
    private def inj(s: GNode, env: Environment[T])          = (s, env)
    private def same(t1: CheckerResult , t2: CheckerResult) = t1 == t2
    
    private def negone(se: StateEnv): Set[StateEnv] = se match { 
        case (s, env) => 
            ((!env).map { case value => (s, value) } ++ root.states.filter(_ != s).map {inj(_, new Bindings)})      
    }

    private def ex_binding(typeOf: TypeOf[T], se: StateEnv) = se._2 match { 
        case Bottom()        => false
        case Bindings(_,neg) => typeOf.filter(Val).size > neg.getOrElse(typeOf.varName,Set()).size
    }
    
    
    //////////////////////////---------------------------------------------///////////////////////////
    //////////////////////////////////////// common functions //////////////////////////////////////// 
    //////////////////////////---------------------------------------------///////////////////////////
    def disj(t1: CheckerResult , t2: CheckerResult) = t1 ++ t2
    
    def Disj(x: Set[CheckerResult]) = x.foldRight(Set[StateEnv]())(disj)
    
    def conj(T1: CheckerResult , T2: CheckerResult) = 
        for (t1 <- T1 ; t2 <- T2 ; inter = interStateEnv(t1,t2) ; if (inter.isDefined)) yield inter.get
        
    def Conj(x: Set[CheckerResult]) = x.foldRight(root.states.map(node => inj(node, new Bindings)))(conj)
        
    def neg(T: CheckerResult) = Conj(T.map(negone))
    
    // s∈ States (Conj {shift(s 0 , T, s) | s 0 ∈ next(s)})
    def preA(T: CheckerResult) = root.states.flatMap(s => Conj(s.next.toSet.map((sNext: GNode) => shift(sNext,T,s))))
    
    def preE(T: CheckerResult) = root.states.flatMap(s => Disj(s.next.toSet.map((sNext: GNode) => shift(sNext,T,s))))
    
    private def SAT_UU(f: CheckerResult => CheckerResult)(T1: CheckerResult , T2: CheckerResult) = {
            var (w,x,y) = (T1,T2,T2)
            do {
                x = y 
                y = disj(y, conj(w, f(y)))
            } while(!same(x,y))           
            y
        }
    
    def SAT_AU = SAT_UU(preA)(_,_)
    
    def SAT_EU = SAT_UU(preE)(_,_)
    
    def exists(typeOf: TypeOf[T], T: CheckerResult) = for (t <- T ; if (ex_binding(typeOf,t))) yield existsone(typeOf.varName,t)
}