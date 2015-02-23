package ast

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.immutable.{ Map => IMap }
import scala.collection.mutable.Set

import ast.model._
import ctl.GraphNode

/**
 * This class performs a conversion from SourceCodeNode to ProgramNode and a transformation from AST to
 * CFG at the same time
 * @author Sofia Boutahar
 * @author Xiaowen Ji
 * @author David Courtinot
 */
class ProgramNodeFactory(rootNodes: Iterable[Decl], labelNodes: Map[String,SourceCodeNode]) {
    type GNode = GraphNode[ProgramNode]
    
    // id used to identify the artificial empty nodes created by the algorithm
    private var currentId = -1
    
    // Map used to create (on the fly) GNode(s) from the SourceCodeNode labels 
    private var labels = Map[String,GNode]()
    private def getLabel(label: String) = labels.getOrElseUpdate(label,toGraphNode(labelNodes(label)))

    lazy val result = 
        Program(IMap(rootNodes.map(decl => decl.name -> {
            val res = handle(decl,None,None,None)
            clean(res,Set())
            res
        }).toSeq: _*))
        
    /**
     * This methods removes all the Empty nodes used for construction and updates the links in
     * consequence. It is called just before returning the result.
     */
    private def clean(node: GNode, explored: Set[GNode]): Unit = {
        if (explored contains node) return

        val (prev,next) = (node.prev.toList,node.next.toList)
        node.value match {
            case Empty(_,_) =>
                prev.foreach { y => y.next -= node; y.next ++= node.next }
                next.foreach { y => y.prev -= node; y.prev ++= node.prev }
            case _ => 
                explored += node
        }
        next.foreach(clean(_,explored))
    }
    
    /**
     * General facade for handling the SourceCodeNode(s)
     */
    def handle(node: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]): GNode = 
        node match {
            case IfStmt(_,_,_)         => handleIf(node,next,exit,entry)
            case ForStmt(_,_,_,_)      => handleFor(node,next,exit,entry)
            case WhileStmt(_,_)        => handleWhile(node,next,exit,entry)
            case DoWhileStmt(_,_)      => handleDoWhile(node,next,exit,entry)
            case CompoundStmt(_)       => handleCompoundStmt(node,next,exit,entry)
            case ReturnStmt(_)         => toGraphNode(node)
            case BreakStmt()           => handleJump(node,exit )
            case ContinueStmt()        => handleJump(node,entry)
            case GotoStmt(label)       => handleJump(node,Some(getLabel(label)))
            case SwitchStmt(_,_)       => handleSwitch(node, next, exit, entry)
            case LabelStmt(_,_)        => handleLabel(node,next,exit,entry)
            case FunctionDecl(_,_,_,_) => handleFunDecl(node,next,exit,entry)
            case DeclStmt(_)           => handleDeclStmt(node,next,exit,entry)
            case _                     => handleNormal(node,next)
    }
    
    private def emptyNode = {
        val res    = new GNode(Empty(CodeRange(-1,-1,-1,-1),currentId.toString))
        currentId -= 1
        res
    }
    private def emptyNode(range: CodeRange, id: String) = new GNode(Empty(range,id))
    
    /**
     * General facade for converting SourceCodeNode to a fresh and unlinked GNode
     */
    private def toGraphNode(node: SourceCodeNode) = (node,node.codeRange.get,node.id.get) match {
        case (ForStmt(init,cond,update,body)     ,range,id) => new GNode(For       (cond,range,id))
        case (DoWhileStmt(cond,body)             ,range,id) => new GNode(While     (cond,range,id))
        case (WhileStmt(cond,body)               ,range,id) => new GNode(While     (cond,range,id))
        case (SwitchStmt(expr,_)                 ,range,id) => new GNode(Switch    (expr,range,id))
        case (IfStmt(expr,_,_)                   ,range,id) => new GNode(If        (expr,range,id))
        case (CaseStmt(expr,_)                   ,range,id) => new GNode(Expression(expr,range,id))
        case (expr: Expr                         ,range,id) => new GNode(Expression(expr,range,id))
        case (CompoundStmt(_)                    ,range,id) => emptyNode                (range,id)
        case (ContinueStmt()                     ,range,id) => emptyNode                (range,id)
        case (LabelStmt(_,_)                     ,range,id) => emptyNode                (range,id)
        case (DefaultStmt(_)                     ,range,id) => emptyNode                (range,id)
        case (BreakStmt()                        ,range,id) => emptyNode                (range,id)
        case (GotoStmt(_)                        ,range,id) => emptyNode                (range,id)
        case (NullStmt()                         ,range,id) => emptyNode                (range,id)
        case (_                                  ,range,id) => new GNode(Statement (node,range,id))
    }
    
    private def handleIf(ifStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = ifStmt match {
       case IfStmt(cond,body,elseStmt) => 
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
            val res = toGraphNode(whileStmt)
            res >> handle(body,Some(res),next,Some(res))
            if (next.isDefined) res >> next.get 
            res
    }
    
    private def handleDoWhile(doWhileStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = doWhileStmt match {
        case DoWhileStmt(condition,body) =>
            val condNode = toGraphNode(doWhileStmt)
            val res      = handle(body,Some(condNode),next,Some(condNode))
            condNode >> res
            if (next.isDefined) condNode >> next.get 
            res
    }
    
    private def handleNormal(node: SourceCodeNode, next: Option[GNode]) = {
        val res = toGraphNode(node)
        if (next.isDefined) res >> next.get
        res
    }
    
    private def handleJump(node: SourceCodeNode, jumpTo: Option[GNode]) = {
        val jump = toGraphNode(node)
        if (jumpTo.isDefined) jump >> jumpTo.get
        jump
    }
    
    private def handleFunDecl(funDecl: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = funDecl match {
        case FunctionDecl(_,_,args,body) => 
            val res        = toGraphNode(funDecl)
            val handleHead = (h: SourceCodeNode, next: Option[GNode]) => handle(h,next,exit,entry) 
            val bodyNode   = handle(body,next,entry,exit)
            val head       = linkElements(args,Some(bodyNode))(handleHead)
            head match {
                case None    => res >> bodyNode
                case Some(x) => res >> x
            }
            res
    }
    
    private def linkElements(list: List[SourceCodeNode], next: Option[GNode])
                            (handleHead: (SourceCodeNode,Option[GNode]) => GNode): Option[GNode] = list match {
        case h :: q => 
            val node = handleHead(h,next)
            q match {
                case Nil => Some(node)
                case _   => linkElements(q,Some(node))(handleHead)
            }
        case Nil => None
    }
    
    private def handleCompoundStmt(cmpdStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = {
        val head = cmpdStmt match {
            case CompoundStmt(elts) =>
                val handleHead = (h: SourceCodeNode, next: Option[GNode]) => handle(h,next,exit,entry) 
                linkElements(elts.reverse,next)(handleHead)
        }
        val res = toGraphNode(cmpdStmt)
        head match {
            case Some(x) => res >> x
            case None    => if (next.isDefined) res >> next.get
        }
        res
    }
    
    private def handleDeclStmt(decl: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]) =  decl match {
        case DeclStmt(decls) =>
            val handleHead = (h: SourceCodeNode, next: Option[GNode]) => handle(h,next,exit,entry) 
            linkElements(decls,next)(handleHead) match {
            	case None    => throw new ConversionFailedException("DeclStmt has to be at least one child")
            	case Some(x) => x
            }
    }
    
    private def handleSwitch(node: SourceCodeNode, nextOpt: Option[GNode], exit: Option[GNode], entry: Option[GNode]) = {
         val res = toGraphNode(node)
         val head = node match {
             case SwitchStmt(expr,CompoundStmt(elts)) =>
                 val handleHead = (h: SourceCodeNode, next: Option[GNode]) => h match {
                     case CaseStmt(_,_) | DefaultStmt(_) => handleCase(h,res,next,nextOpt,entry)
                     case _                              => handle(h,next,nextOpt,entry)
                 }
                 linkElements(elts.reverse,nextOpt)(handleHead)
        }
        
        head match {
            case Some(x) => res >> x
            case None    => if (nextOpt.isDefined) res >> nextOpt.get
        }
        res
    }
     
    private def handleCase(caseStmt: SourceCodeNode, prev: GNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]): GNode = {
        val res = toGraphNode(caseStmt)
        caseStmt match {
            case CaseStmt(_, body) => 
                body match {
                    case CaseStmt(_,_)  => res >> handleCase(body,prev,next,exit,entry)
                    case BreakStmt()    => res >> exit.get
                    case DefaultStmt(_) => res >> handleCase(body,prev,next,exit,entry)
                    case _              => res >> handle(body,next,exit,entry)
                }
            case DefaultStmt(body) => res >> handle(body,next,exit,entry)
        }
        prev >> res
    }
    
    private def handleLabel(labelStmt: SourceCodeNode, next: Option[GNode], exit: Option[GNode], entry: Option[GNode]): GNode = labelStmt match {
        case LabelStmt(label, body) =>
            val res = getLabel(label)
            res >> handle(body, next, exit, entry)
            res
    }
}
