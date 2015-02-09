package ast

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set

import ast.model._
import cfg.GraphNode

class ProgramNodeFactory(rootNode: SourceCodeNode, labelNodes: Map[String,SourceCodeNode]) {
    type GNode = GraphNode[ProgramNode,ProgramNodeLabelizer]
    
    // id used to identify the artificial empty nodes created by the algorithm
    private var currentId = 0
    // Map used to create (on the fly) GNode(s) from the SourceCodeNode labels 
    private var labels = Map[String,GNode]()
    private def getLabel(label: String) = labels.getOrElseUpdate(label,toGraphNode(labelNodes(label)))
    
    lazy val result = 
        clean(
            handle(rootNode,None,None,None)
            ,Set()).head
    
    private val debugEnabled = false
    private def debug(msg: String) = if (debugEnabled) println(msg)
            
    private def clean(node: GNode, explored: Set[GNode]): List[GNode] = {
        if (!explored.contains(node)) {
            explored += node
            node.value match {
                case Empty(_,_) => 
                    val res = (node.prev,node.next) match {
                        case (Nil,next)  => node >>>/ next; next.flatMap(clean(_,explored))
                        case (prev,Nil)  => node /<<< prev; prev
                        case (prev,next) =>
                        	node >>>/ next
                            val cleaned = next.flatMap(clean(_,explored))
                            for (x <- prev ; y <- cleaned) x >> y
                            node /<<< prev
                            prev
                    }
                    res
                case _ => node.next.foreach(clean(_,explored)); List(node)
            }
        } else List(node)
    }
    
    def handle(node: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]): GNode = 
		node match {
	        case IfStmt(_,_,_)       => handleIf(node,next,exit,entry)
	        case ForStmt(_,_,_,_)    => handleFor(node,next,exit,entry)
	        case WhileStmt(_,_)      => handleWhile(node,next,exit,entry)
	        case DoWhileStmt(_,_)    => handleDoWhile(node,next,exit,entry)
	        case CompoundStmt(_)     => handleCompoundStmt(node,next,exit,entry)
	        case BreakStmt()         => handleJump(node,exit )
	        case ContinueStmt()      => handleJump(node,entry)
	        case GotoStmt(label)     => handleJump(node,Some(getLabel(label)))
	        case FunctionDecl(_,_,_) => handleFunDecl(node,next,exit,entry)
	        case _                   => handleDefault(node,next)
    }
    
    
    private def emptyNode = {
        val res    = new GNode(Empty(CodeRange(-1,-1,-1,-1),currentId.toString))
        currentId -= 1
        res
    }
    private def emptyNode(range: CodeRange, id: String) = new GNode(Empty(range,id))
    private def toGraphNode(node: SourceCodeNode) = (node,node.codeRange.get,node.id.get) match {
        case (ForStmt     (init,cond,update,body),range,id) => new GNode(For       (cond,range,id))
        case (WhileStmt   (cond,body)            ,range,id) => new GNode(While     (cond,range,id))
        case (DoWhileStmt (cond,body)            ,range,id) => new GNode(While     (cond,range,id))
        case (IfStmt(expr,_,_)                   ,range,id) => new GNode(If        (expr,range,id))
        case (expr: Expr                         ,range,id) => new GNode(Expression(expr,range,id))
        case (CompoundStmt(_)                    ,range,id) => emptyNode                (range,id)
        case (BreakStmt()                        ,range,id) => emptyNode                (range,id)
        case (ContinueStmt()                     ,range,id) => emptyNode                (range,id)
        case (FunctionDecl(_,_,_)                ,range,id) => emptyNode                (range,id)
        case (NullStmt()                         ,range,id) => emptyNode                (range,id)
        case (stmt: Stmt                         ,range,id) => new GNode(Statement (stmt,range,id))
    }
    
    private def handleCompoundStmt(cmpdStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = {
        debug("handleCompound(\n\t%s\n\t%s\n\t%s\n\t%s)".format(cmpdStmt,next,exit,entry))
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
           debug("handleIf(\n\t%s\n\t%s\n\t%s\n\t%s)".format(ifStmt,next,exit,entry))
           val ifNode = toGraphNode(ifStmt)
           ifNode >> handle(body,next,exit,entry)
           elseStmt match {
               case None    => if (next.isDefined) ifNode >> next.get
               case Some(x) => ifNode >> handle(x,next,exit,entry) 
           }
           ifNode
    }
	
    private def handleFor(forStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = forStmt match {
        case ForStmt(init,cond,update,body) => 
            debug("handleFor(\n\t%s\n\t%s\n\t%s\n\t%s)".format(cond,next,exit,entry))
            val condNode   = toGraphNode(forStmt)
            val initNode   = if (init  .isDefined) toGraphNode(init  .get) else emptyNode
            val updateNode = if (update.isDefined) toGraphNode(update.get) else emptyNode
            val bodyNode   = handle(body,Some(updateNode),next,Some(condNode))
            
            initNode   >> condNode >> bodyNode
            updateNode >> condNode
            if (next.isDefined) condNode >> next.get
            initNode
    }
	
	private def handleWhile(whileStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = whileStmt match {
	    case WhileStmt(condition,body) =>
	        debug("handleWhile(\n\t%s\n\t%s\n\t%s\n\t%s)".format(whileStmt,next,exit,entry))
	        val res = toGraphNode(whileStmt)
            res >> handle(body,Some(res),next,Some(res))
            if (next.isDefined) res >> next.get 
            res
	}
	
	private def handleDoWhile(doWhileStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = doWhileStmt match {
	    case DoWhileStmt(condition,body) =>
	        debug("handleDoWhile(\n\t%s\n\t%s\n\t%s\n\t%s)".format(doWhileStmt,next,exit,entry))
	        val condNode = toGraphNode(doWhileStmt)
	        val res      = handle(body,Some(condNode),next,Some(condNode))
            condNode >> res
            if (next.isDefined) condNode >> next.get 
            println(res)
            res
	}
	
    private def handleDefault(node: SourceCodeNode, next: Option[GNode]) = {
        debug("handleDefault(\n\t%s\n\t%s)".format(node,next))
        val res = toGraphNode(node)
        if (next.isDefined) res >> next.get
        res
    }
    
    private def handleJump(node: SourceCodeNode, jumpTo: Option[GNode]) = {
        debug("handleJump(\n\t%s\n\t%s)".format(node,jumpTo))
        val jump = toGraphNode(node)
        if (jumpTo.isDefined) jump >> jumpTo.get
        jump
    }
    
    private def handleFunDecl(funDecl: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = funDecl match {
    	case FunctionDecl(_,_,body) => 
    	    debug("handleFunDecl(\n\t%s\n\t%s\n\t%s\n\t%s)".format(funDecl,next,exit,entry))
    	    val res = toGraphNode(funDecl)
    	    res >> handle(body,next,entry,exit)
    	    res
    }
}