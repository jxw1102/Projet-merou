package ast

import cfg.GraphNode

object ProgramNodeFactory {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
	def handleSourceCodeNode(node: SourceCodeNode): (GNode,Set[GNode]) = node match {
	    case IfStmt(condition,body,elseStmt) => handleIf(node.asInstanceOf[IfStmt])
	        
	    
	}

	def handleIf(ifStmt: IfStmt) = ifStmt match {
	    case IfStmt(condition,body,elseStmt) => 
	        val res   = new GNode(If(condition))
	        val left  = handleSourceCodeNode(body) 
	        
//	        val 
	        res.addNext(left)
	        if (elseStmt.isDefined) res.addNext(handleSourceCodeNode(elseStmt.get))
	        res
	}
	
	def handleWhile()
}