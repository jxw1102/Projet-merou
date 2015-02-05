package ast

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import ast.model._
import cfg.GraphNode
import scala.collection.mutable.ArrayBuffer

//class ProgramNodeFactory(nodes: List[SourceCodeNode], val jumps: Map[Long,Long]) {
class ProgramNodeFactory(rootNode: SourceCodeNode, labelNodes: Map[String,SourceCodeNode]) {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
    /**
     * Starts by converting every SourceCodeNode into a GraphNode while accumulating information about jump statements.
     * To finish, finalizes the graph by linking the jump statements origin(s) to their destination
     */
    
    lazy val result = handle(rootNode, NullStmt(), None, None)

    // the returned set must NEVER contain a Jump
    def handle(node: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]): SourceCodeNode = {
        val res = node match {
        	case IfStmt(_,_,_)                     => handleIf(node,next,exit,entry)
	    	case ForStmt(_,_,_,_)                  => handleFor(node,next,exit,entry)
        	case WhileStmt(_,_)                    => handleWhile(node,next,exit,entry)
            case CompoundStmt(_)                   => handleCompoundStmt(node,next,exit,entry)
            case NullStmt()                        => node
            case BreakStmt()                       => handleJumpStmt(node,exit.get)
            case ContinueStmt()                    => handleJumpStmt(node,entry.get)
            case GotoStmt(label)                   => handleJumpStmt(node,labelNodes(label))
            case _                                 => handleExpr(node,next)
        }
        res
    }
    
    private def toGraphNode(node: SourceCodeNode) = (node,node.codeRange.get,node.id.get) match {
        case (ForStmt(init,cond,update,body),range,id) => new GNode(For(cond,range,id))
        case (ForStmt(init,cond,update,body),range,id) => new GNode(For(cond,range,id))
        
    }
    
    private def handleCompoundStmt(cmpdStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = cmpdStmt match {
        case CompoundStmt(elts) =>
            val nexts = elts.drop(1) :+ NullStmt()
            for(i <- elts.zip(nexts)) i match {
                case (a,b) => handle(a,b,exit,entry)
            }
            cmpdStmt >> elts(0)
            next match {
                case NullStmt() =>    
                case _          =>    elts.last >> next 
            }
            cmpdStmt
    }
    
	private def handleIf(ifStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = ifStmt match {
       case IfStmt(condition,body,elseStmt) => 
           ifStmt >> condition
           next match {
                case NullStmt() =>    
                case _          =>
                    condition >> handle(body, next, exit, entry)
                    body >> next
                    condition >> handle(elseStmt.get, next, exit, entry)
                    elseStmt.get >> next
            }
           ifStmt
    }
	
	private def tryToLink(node: GNode, next: Option[SourceCodeNode]) = next match {
		case None    => node
	    case Some(x) => val res = toGraphNode(x); node >> res; res
	}
    
    private def handleFor(forStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = forStmt match {
        case ForStmt(init,cond,update,body) => 
            val res  = toGraphNode(forStmt)
        	var next = tryToLink(res,init)
            next     = tryToLink(next,cond)
            next     = tryToLink(next,body)
            next     = tryToLink(next,update)
            next     = tryToLink(next,cond)
        	res
    }
	
	private def handleWhile(whileStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = whileStmt match {
	    case WhileStmt(condition,body) =>
            whileStmt >> condition
            condition >> handle(body.get,condition,Some(next),Some(condition))
            next match {
                case NullStmt() =>    
                case _          =>    condition >> next
            }
            whileStmt
	}
    
    private def handleExpr(node: SourceCodeNode, next: SourceCodeNode) = {
        next match {
                case NullStmt() =>    
                case _          =>    node >> next
        }
        node
    }
    
    private def handleJumpStmt(node: SourceCodeNode, exit: SourceCodeNode) = {
        node >> exit
        node
    }
    
}
