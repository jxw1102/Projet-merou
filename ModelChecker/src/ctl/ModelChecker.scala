package ctl

import scala.reflect.runtime.universe._

/**
 * The ModelChecker implements the model-checking algorithm and is able to apply CTL expressions on a graph. To construct 
 * the ModelChecker, you need to pass it a GraphNode from which all the nodes can be accessed using only the successors
 * to traverse the graph, and a conversion function returning for a given node (or more precisely, the value held by this node)
 * the set of values this node is likely to introduce in the environment while evaluating a Predicate on it.
 * @author Zohour Abouakil
 * @author David Courtinot
 */
class ModelChecker[M <: MetaVariable: TypeTag, N, V <: Value: TypeTag](private val root: GraphNode[N], convert: N => Set[V]) {
    private type StateEnv      = (GNode, Environment[M,V])
    private type GNode         = GraphNode[N]
    private type CheckerResult = Set[StateEnv]
    
    // Val is the set of all possible values for a meta-variable
    private lazy val Val: Set[V] = root.states.flatMap(g => convert(g.value)).toSet
    
    /**
     * This method evaluates a CTL expression on the graph
     * @return a set of (node,environment) pairs corresponding to all the nodes matching a given
     *         CTL expression
     */
    def evalExpr(expr: CtlExpr[M,N,V]): CheckerResult = expr match {
            case And       (x, y) => conj  (evalExpr(x),evalExpr(y))
            case Or        (x, y) => disj  (evalExpr(x),evalExpr(y))
            case AU        (x, y) => SAT_AU(evalExpr(x),evalExpr(y))
            case EU        (x, y) => SAT_EU(evalExpr(x),evalExpr(y))
            case AX        (x   ) => preA  (evalExpr(x))
            case EX        (x   ) => preE  (evalExpr(x))
            case Not       (x   ) => neg   (evalExpr(x))
            case Exists    (x, y) => exists(x,evalExpr(y))
            case Predicate (x   ) => root.states.flatMap(n => x.test(n.value).map((n,_)))
    }
    
    private[ctl] def interStateEnv(se1: StateEnv, se2: StateEnv): Option[StateEnv] = {
        if (se1._1 != se2._1) None
        else se1._2 & se2._2 match {
            case BindingsEnv(b) => Some((se1._1,BindingsEnv(b)))
            case _ => None
        }              
    }
    
    private[ctl] def existsone(metaData: M, ev: StateEnv): StateEnv   = (ev._1,ev._2 - metaData)
    private[ctl] def inj      (s: GNode, env: Environment[M,V])       = (s,env)
    private[ctl] def same     (t1: CheckerResult , t2: CheckerResult) = t1 == t2
    private[ctl] def negone   (se: StateEnv): Set[StateEnv]           = se match { 
        case (s, env) => 
            (!env).map { case value => (s, value) } ++ (root.states - s).map(inj(_, new BindingsEnv[M,V]))    
    }

    private[ctl] def ex_binding(varName : M, typeOf: TypeOf[V], se: StateEnv) = se._2 match { 
        case BindingsEnv(bind) => bind.get(varName) match {
            case Some(NegBinding(neg)) => typeOf.filter(Val).size > neg.size
            case _                     => true
        }        
        case _ => false
    }
    
    private[ctl] def conj(T1: CheckerResult , T2: CheckerResult) = 
        for (t1 <- T1 ; t2 <- T2 ; inter = interStateEnv(t1,t2) ; if (inter.isDefined)) yield inter.get
        
    private[ctl] def disj    (t1: CheckerResult , t2: CheckerResult)    = t1 ++ t2
    private[ctl] def shift   (s1: GNode , T: CheckerResult, s2: GNode)  = T.filter { case(a,b) => a == s1 }.map{ case(a,b) => (s2,b) }
    private[ctl] def neg     (T: CheckerResult)                         = conjFold(T.map(negone))
    private[ctl] def exists  (varType: (M,TypeOf[V]), T: CheckerResult) = for (t <- T ; if (ex_binding(varType._1, varType._2,t))) yield existsone(varType._1,t)
    private[ctl] def Disj    (x: Set[CheckerResult])                    = x.foldLeft(Set[StateEnv]())(disj)
    private[ctl] def conjFold(x: Set[CheckerResult]): CheckerResult     = x.foldLeft(root.states.map(node => inj(node, new BindingsEnv)))(conj)
    
    private[ctl] def preA(T: CheckerResult) = root.states.flatMap(s => conjFold(s.next.toSet.map((sNext: GNode) => shift(sNext,T,s))))   
    private[ctl] def preE(T: CheckerResult) = root.states.flatMap(s => Disj(s.next.toSet.map((sNext: GNode) => shift(sNext,T,s))))   
    private[ctl] def SAT_AU                 = SAT_UU(preA)(_,_)
    private[ctl] def SAT_EU                 = SAT_UU(preE)(_,_)
    
    // this methods enables to factorize the code of SAT_AU and SAT_EU
    private[ctl] def SAT_UU(f: CheckerResult => CheckerResult)(T1: CheckerResult , T2: CheckerResult) = {
        var (w,y,x) = (T1,T2,T2)
        do {
            x = y 
            y = disj(y, conj(w, f(y)))
        } while(!same(x,y))           
        y
    }
}