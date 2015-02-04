package ast

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import ast.model._
import cfg.GraphNode

class ProgramNodeFactory(nodes: List[SourceCodeNode], val jumps: Map[Long,Long]) {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
    type MHSet = scala.collection.mutable.HashSet[GNode]
    
    /**
     * The keys of this map are the ids of the JumpSmt(s) (break, continue, goto) in the code.
     * The values are, for each JumpStmt, the set of reachable states just before the JumpStmt
     * is executed
     */
    private val jumpPredecessors = Map[Long,Set[GNode]]()
    
    /** 
     * The keys of this map are the ids of the statements that are the target of a JumpStmt .
     * The values are their conversion to ProgramNode. This will help to finalize the graph by linking every predecessor
     * of a JumpStmt to its target
     */
    private val convertedStmts = Map[Long,GNode]()
    
    // the graph is lazily computed
    lazy val graph = toGraph(nodes)
    
    /**
     * Starts by converting every SourceCodeNode into a GraphNode while accumulating information about jump statements.
     * To finish, finalizes the graph by linking the jump statements origin(s) to their destination
     */
    private def toGraph(nodes: List[SourceCodeNode]) = {
        ???
        finalizeLinks
    }
    
    /**
     * Links the predecessors of the jump statements using 'jumpPredecessors', 'convertedStmts' and 'jumps'
     */
    private def finalizeLinks = ???

    def handleSourceCodeNode(node: SourceCodeNode): (GNode,Set[GNode]) = node match {
        case IfStmt(condition,body,elseStmt)   => handleIf(node.asInstanceOf[IfStmt])
//        case ForStmt(init, cond, update, body) => handleFor(node.asInstanceOf[ForStmt])
//        case WhileStmt(condition, body)        => handleWhile(node.asInstanceOf[WhileStmt])
//        case DoWhileStmt(condition, body)      => handleDoWhile(node.asInstanceOf[DoWhileStmt])
        
    }
    
   def handleCompoundStmt(comp: CompoundStmt, outSet: Set[GNode]) = {
       
   }
    
    def handleExpression(node: Expr, outSet: Set[GNode]) = {
        val res = Expression(node,node.codeRange.get,node.id.get)
        (res,outSet)
    }
    
    def handleLoopBody(node: CompoundStmt, outSet: Set[GNode]) = {
        var (in,out) = handleSourceCodeNode(node.elts(0))
        node.elts.drop(1).foreach {
            case x: LoopStmt => ???
            case BreakStmt() => outSet ++= out
            case x           => 
                val (newIn,newOut) = handleSourceCodeNode(x)
                
        }
    }
    
    def handleIf(ifStmt: IfStmt) = ifStmt match {
       case IfStmt(condition,body,elseStmt) => 
            val res   = new GNode(If(condition,ifStmt.codeRange.get,ifStmt.id.get))
            val left  = handleSourceCodeNode(body) 
            val out   = new MHSet()
            out     ++= left._2 
            res      >> left._1
            
            elseStmt match {
                case Some(x) => 
                    val right = handleSourceCodeNode(x)
                    out     ++= right._2
                    res      >> right._1
                case None    =>
            }
            (res,out)
    }
    
    
    def handleWhile(whileStmt: WhileStmt) = whileStmt match {
        case WhileStmt(condition,body) =>
            val res = new GNode(While(condition,whileStmt.codeRange.get,whileStmt.id.get))
            val in  = handleSourceCodeNode(body)
            
    }
}