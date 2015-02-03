package ast

import cfg.GraphNode
import scala.collection.mutable.Set

object ProgramNodeFactory {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
    type MHSet = scala.collection.mutable.HashSet[GNode]
	def handleSourceCodeNode(node: SourceCodeNode): (GNode,Set[GNode]) = node match {
	    case IfStmt(condition,body,elseStmt)   => handleIf(node.asInstanceOf[IfStmt])
	    case ForStmt(init, cond, update, body) => handleFor(node.asInstanceOf[ForStmt])
        case WhileStmt(condition, body)        => handleWhile(node.asInstanceOf[WhileStmt])
        case DoWhileStmt(condition, body)      => handleDoWhile(node.asInstanceOf[DoWhileStmt])
        
	    
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
	
	def handleWhile(whileStmt: WhileStmt){
        
    }
    
    def handleDoWhile(doWhileStmt: DoWhileStmt){
        
    }
    
    def handleFor(whileStmt: WhileStmt){
        
    }
    
}