package ast

import cfg.GraphNode
import scala.collection.mutable.Set
import collection.mutable.Map
import ast.model.IfStmt
import ast.model.CompoundStmt
import ast.model.WhileStmt
import ast.model.BreakStmt
import ast.model.LoopStmt

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
        handleElements(nodes)
        finalizeLinks
    }
    
    /**
     * Links the predecessors of the jump statements using 'jumpPredecessors', 'convertedStmts' and 'jumps'
     */
    private def finalizeLinks = ???
    
    private def saveAndReturn(node: SourceCodeNode, converted: GNode) = { convertedStmts += node.id.get -> converted; converted }

    // the returned set must NEVER contain a Jump
	private def handleSourceCodeNode(node: SourceCodeNode): (GNode,Set[GNode]) = {
        val res = node match {
        	case IfStmt(condition,body,elseStmt)   => handleIf(node.asInstanceOf[IfStmt])
//	    	case ForStmt(init, cond, update, body) => saveAndReturn(node,handleFor(node.asInstanceOf[ForStmt]))
//        	case WhileStmt(condition, body)        => handleWhile(node.asInstanceOf[WhileStmt])
//      	  case DoWhileStmt(condition, body)      => handleDoWhile(node.asInstanceOf[DoWhileStmt])
        
        }
        res
    }
    
	private def handleElements(elts: List[SourceCodeNode]) = {
    	val first = handleSourceCodeNode(elts(0))
    	val convert = elts.drop(1).map(handleSourceCodeNode)
    	// connect each entry point b._1 to the set of exits (in a._2) of the previous element
    	
    	(first :: convert).zip(convert).foreach { 
    	    case (a,b) => b._1.value match { 
    	        case x: Jump => jumpPredecessors += x.id -> a._2 
    	        case _       => b._1 <<< a._2
    	    }
    	}
	}
	
    private def handleCompoundStmt(node: CompoundStmt) = handleElements(node.elts)
    
	private def handleIf(ifStmt: IfStmt) = ifStmt match {
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
	
	
	private def handleWhile(whileStmt: WhileStmt) = whileStmt match {
	    case WhileStmt(condition,body) =>
	        val res = new GNode(While(condition,whileStmt.codeRange.get,whileStmt.id.get))
	        val in  = handleSourceCodeNode(body)
	        
	}
	
//	private
}

private
