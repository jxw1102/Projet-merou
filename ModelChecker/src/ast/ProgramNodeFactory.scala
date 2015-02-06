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
	        case BreakStmt()                       => handleJump(node,next,exit )
	        case ContinueStmt()                    => handleJump(node,next,entry)
//	        case GotoStmt(label)                   => handleJumpStmt(node,labelNodes(label))
	        case _                                 => handleDefault(node,next)
    }
    
    private def toGraphNode(node: SourceCodeNode) = (node,node.codeRange.get,node.id.get) match {
        case (ForStmt     (init,cond,update,body),range,id) => new GNode(For       (cond,range,id))
        case (WhileStmt   (cond,body)            ,range,id) => new GNode(While     (cond,range,id))
        case (CompoundStmt(_)                    ,range,id) => new GNode(Empty     (     range,id))
        case (IfStmt(expr,_,_)                   ,range,id) => new GNode(If        (expr,range,id))
        case (expr: Expr                         ,range,id) => new GNode(Expression(expr,range,id))
        case (BreakStmt()                        ,range,id) => new GNode(Empty     (     range,id))
        case (ContinueStmt()                     ,range,id) => new GNode(Empty     (     range,id))
        case (stmt: Stmt                         ,range,id) => new GNode(Statement (stmt,range,id))
    }
    
    private def handleCompoundStmt(cmpdStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = {
        val head = cmpdStmt match {
	        case CompoundStmt(elts) =>
	            def linkElements(list: List[SourceCodeNode], next: Option[GNode]): Option[GNode] = list match {
	            	case h :: q   => 
	            	    val node = handle(h,next,exit,entry) 
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
	
	private def tryToLink(node: Option[GNode], next: Option[GNode]) = (node,next) match {
	    case (Some(x),Some(y)) => x >> y; (Some(x),Some(y))
	    case (None   ,Some(y)) =>         (Some(y),Some(y))
	    case (Some(x),None   ) =>         (Some(x),Some(x))
	    case (None   ,None   ) =>         (None   ,None   )
	}
    
    private def handleFor(forStmt: SourceCodeNode, nextOpt: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = forStmt match {
        case ForStmt(init,cond,update,body) => 
//        	val (res,cond) = tryToLink(init,toGraphNode(forStmt),exit,entry)
//            val bodyNode   = tryToLink(cond,body,exit,entry)._2
//            var next       = tryToLink(bodyNode,update,exit,entry)._2
//            next >> cond
//        	res
	        val condNode   = Some(toGraphNode(forStmt))
            val initNode   = init   match { case Some(x) => Some(toGraphNode(x))                        case None => None }
        	val updateNode = update match { case Some(x) => Some(toGraphNode(x))                        case None => None }
        	val bodyNode   = body   match { 
        	    case Some(x) => Some(handle(x,if (updateNode.isDefined) updateNode else condNode,nextOpt,condNode))  
        	    case None    => None
        	}
        	
        	var (res,next) = tryToLink(initNode,condNode)
        	next           = tryToLink(next    ,bodyNode)._2 
        	if (updateNode.isDefined) 
        	    updateNode.get >> condNode.get 
        	res.get
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
    
    private def handleJump(node: SourceCodeNode, res: Option[GNode], jumpTo: Option[GNode]) = {
        val jump = toGraphNode(node)
        if (jumpTo.isDefined) jump >> jumpTo.get
        if (res.isDefined) res.get else jump 
    }
    
}
