package ast

import cfg.GraphNode
import scala.collection.mutable.Set

object ProgramNodeFactory {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
    type MHSet = scala.collection.mutable.HashSet[GNode]
    
	def handleSourceCodeNode(node: SourceCodeNode): (GNode,Set[GNode]) = node match {
	    case IfStmt   (condition,body,elseStmt) => handleIf   (node.asInstanceOf[IfStmt])
	    case WhileStmt(condition,body)          => handleWhile(node.asInstanceOf[WhileStmt])
	       
	    
	}

    def handleLoopBody(node: CompoundStmt, outSet: Set[GNode]) = {
        var (in,out) = handleSourceCodeNode(node.elts(0))
        node.elts.drop(1).foreach {
            case x: LoopStmt => ???
            case BreakStmt() => outSet ++= out
            case x           => 
                val (newIn,newOut) = handleSourceCodeNode(x)
                newIn.
        }
    }
    
	def handleIf(ifStmt: IfStmt) = ifStmt match {
	    case IfStmt(condition,body,elseStmt) => 
	        val res   = new GNode(If(condition))
	        val left  = handleSourceCodeNode(body) 
	        val out   = new MHSet()
	        out     ++= left._2 
	        res.addNext(left._1)
	        
	        elseStmt match {
	            case Some(x) => 
	                val right = handleSourceCodeNode(x)
	                out     ++= right._2
	                res.addNext(right._1)
	            case None    =>
	        }
	        (res,out)
	}
	
	def handleWhile(whileStmt: WhileStmt) = whileStmt match {
	    case WhileStmt(condition,body) =>
	        val res = new GNode(While(condition))
	        val in  = handleSourceCodeNode(body)
	        
	}
}