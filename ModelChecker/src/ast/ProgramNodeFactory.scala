package ast

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

import ast.model._
import cfg.GraphNode

class ProgramNodeFactory(rootNode: SourceCodeNode, labelNodes: Map[String,SourceCodeNode]) {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
    
    lazy val result = handle(rootNode,None,None,None)
    
    def handle(node: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]): GNode = 
		node match {
	        case IfStmt(_,_,_)                     => handleIf(node,next,exit,entry)
	        case ForStmt(_,_,_,_)                  => handleFor(node,next,exit,entry)
	        case WhileStmt(_,_)                    => handleWhile(node,next,exit,entry)
	        case CompoundStmt(_)                   => handleCompoundStmt(node,next,exit,entry)
//	        case BreakStmt()                       => handleJumpStmt(node,exit.get)
//	        case ContinueStmt()                    => handleJumpStmt(node,entry.get)
//	        case GotoStmt(label)                   => handleJumpStmt(node,labelNodes(label))
	        case _                                 => handleDefault(node,next)
    }
    
    private def toGraphNode(node: SourceCodeNode) = (node,node.codeRange.get,node.id.get) match {
        case (ForStmt     (init,cond,update,body),range,id) => new GNode(For       (cond,range,id))
        case (WhileStmt   (cond,body)            ,range,id) => new GNode(While     (cond,range,id))
        case (CompoundStmt(_)                    ,range,id) => new GNode(Block     (     range,id))
        case (IfStmt(expr,_,_)                   ,range,id) => new GNode(If        (expr,range,id))
        case (expr: Expr                         ,range,id) => new GNode(Expression(expr,range,id))
        case (stmt: Stmt                         ,range,id) => new GNode(Statement (stmt,range,id))
    }
    
    private def handleCompoundStmt(cmpdStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = {
        val head = cmpdStmt match {
	        case CompoundStmt(elts) =>
	            def linkElements(list: List[SourceCodeNode], next: Option[GNode]): Option[GNode] = list match {
	            	case h :: q   => 
	            	    val node = handle(h,next,exit,entry) 
//	            	    if (next.isDefined) node >> next.get
	            	    q match {
	            	        case Nil => Some(node)
	            	        case _   => linkElements(q,Some(node))
	            	    }
	                case Nil => None
	            }
	            linkElements(elts.reverse,next)
        }
        val res = toGraphNode(cmpdStmt)
        head match {
            case Some(x) => res >> x
            case None    => if (next.isDefined) res >> next.get
        }
        res
    }
    
	private def handleIf(ifStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = ifStmt match {
       case IfStmt(cond,body,elseStmt) => 
           val ifNode   = toGraphNode(ifStmt)
           val condNode = toGraphNode(cond)
           ifNode >> condNode
           
           // link the body of the then branch
           val bodyNode = handle(body,next,exit,entry)
           condNode >> bodyNode
           if (next.isDefined) bodyNode >> next.get
           
           // link the body of the else branch (if any)
           elseStmt match {
        	   case None    =>
        	   case Some(x) =>
        	   val elseNode = handle(x,next,exit,entry)
        	   condNode >> elseNode
        	   if (next.isDefined) elseNode >> next.get
           }
           ifNode
    }
	
	private def tryToLink(node: GNode, next: Option[SourceCodeNode]) = next match {
		case None    => (node,node)
	    case Some(x) => val res = toGraphNode(x); node >> res; (node,res)
	}
	
	private def tryToLink(node: Option[SourceCodeNode], next: GNode) = node match {
		case None    => (next,next)
	    case Some(x) => val res = toGraphNode(x); res >> next; (res,next)
	}
    
    private def handleFor(forStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = forStmt match {
        case ForStmt(init,cond,update,body) => 
        	val (res,cond) = tryToLink(init,toGraphNode(forStmt))
        	// incorrect here, must call handle on the body
            var next       = tryToLink(cond,body)._2
            next           = tryToLink(next,update)._2
            next >> cond
        	res
    }
	
	private def handleWhile(whileStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = whileStmt match {
	    case WhileStmt(condition,body) =>
	        val res      = toGraphNode(whileStmt)
	        val condNode = toGraphNode(condition)
	        
            res      >> condNode
            condNode >> handle(body.get,Some(condNode),next,Some(condNode))
            next match {
                case None    =>    
                case Some(x) => condNode >> x
            }
            res
	}
	
    private def handleDefault(node: SourceCodeNode, next: Option[GNode]) = {
        val res = toGraphNode(node)
        if (next.isDefined) res >> next.get
        res
    }
    
//    private def handleJumpStmt(node: SourceCodeNode, exit: SourceCodeNode) = {
//        node >> exit
//        node
//    }
    
}
