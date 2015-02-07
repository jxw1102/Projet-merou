package ast

import scala.collection.mutable.Map

import ast.model._
import ast.model.Expr
import cfg.GraphNode

//class ProgramNodeFactory(nodes: List[SourceCodeNode], val jumps: Map[Long,Long]) {
class ProgramNodeFactory(rootNode: SourceCodeNode, labelNodes: Map[String,SourceCodeNode]) {    
    /**
     * Starts by converting every SourceCodeNode into a GraphNode while accumulating information about jump statements.
     * To finish, finalizes the graph by linking the jump statements origin(s) to their destination
     */
    
    lazy val result = handle(rootNode,NullStmt(),None,None)

    def handle(node: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]): SourceCodeNode = {
        val res = node match {
        	case IfStmt(_,_,_)                     => handleIf(node,next,exit,entry)
	    	case ForStmt(_,_,_,_)                  => handleFor(node,next,exit,entry)
        	case WhileStmt(_,_)                    => handleWhile(node,next,exit,entry)
            case CompoundStmt(_)                   => handleCompound(node,next,exit,entry)
            case NullStmt() | ReturnStmt(_)        => node
            case BreakStmt()                       => handleJump(node,exit.get)
            case ContinueStmt()                    => handleJump(node,entry.get)
            case GotoStmt(label)                   => handleJump(node,labelNodes(label))
            case SwitchStmt(_,_)                   => handleSwitch(node,next,Some(next),entry)
            case CaseStmt(_,_)                     => handleSwitchCase(node,next,exit,entry)
            case DefaultStmt(_)                    => handleSwitchCase(node,next,exit,entry)
            case _                                 => handleExpr(node,next)
        }
        res
    }
    
    private def findLeaves(node: SourceCodeNode): List[SourceCodeNode] = findLeaves(Some(node))
    private def findLeaves(node: Option[SourceCodeNode]): List[SourceCodeNode] = node match {
        case Some(x) => x match {
            case CompoundStmt(elts)         => if (elts.isEmpty) List(x) else findLeaves(elts.last)
            case IfStmt(cond,body,elseStmt) => findLeaves(body) ++ findLeaves(elseStmt)
            case BreakStmt()                => List()
            case ReturnStmt(_)              => List()
            case _                          => List(x)
        }
        case None    => List()
    }
    
    private def handleCompound(cmpdStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = cmpdStmt match {
        case CompoundStmt(elts) =>
            if (elts.length > 0) {
                val nexts = elts.drop(1) :+ NullStmt()
                for(i <- elts.zip(nexts)) i match {
                    case (a,b) => handle(a,b,exit,entry)
                }
                cmpdStmt >> elts(0)
                findLeaves(elts.last).foreach(_.>>(next))
            }
            cmpdStmt
    }
    
	private def handleIf(ifStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = ifStmt match {
       case IfStmt(condition,body,elseStmt) =>
            ifStmt >> condition
            condition >> handle(body, next, exit, entry)
            elseStmt match {
                case Some(x) => condition >> handle(x, next, exit, entry)
                case None    => if (next.isInstanceOf[NullStmt]) condition >> exit else condition >> next
            }
            ifStmt
    }
    
    private def handleFor(forStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = forStmt match {
        case ForStmt(init,cond,update,body) =>
            forStmt >> init >> cond >> handle(body,(update or cond or body).get,Some(next),(update or cond))
            body match {
                case NullStmt()                         => (cond or init or forStmt).get >> update >> (cond or forStmt)
                case CompoundStmt(elts) if elts.isEmpty => body >> update >> (cond or body)
                case _                                  => (cond or body).get << update
            }
            next << (cond or init or forStmt)
            val cb = (cond or body)
            forStmt
    }
	
	private def handleWhile(whileStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = whileStmt match {
	    case WhileStmt(condition,body) =>
            whileStmt >> condition
            condition >> handle(body,condition,Some(next),Some(condition))
            condition >> next
            body match {
                case NullStmt()                         => condition >> condition
                case CompoundStmt(elts) if elts.isEmpty => body >> condition
                case _                                  => 
            }
            whileStmt
	}
    
    private def handleSwitch(switchStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = switchStmt match {
        case SwitchStmt(expr,body) =>
            switchStmt >> expr
            expr >> handleSwitchBody(body,next,exit,entry)
            switchStmt
    }
    
    private def handleSwitchBody(switchBody: CompoundStmt, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = switchBody match {
        case CompoundStmt(elts) =>
            val nexts = elts.drop(1) :+ NullStmt()
            for(i <- elts.zip(nexts)) i match {
                case (a,b) => 
                    a match { 
                        case CaseStmt(_,_) | DefaultStmt(_) => handleSwitchBranch(a,switchBody)
                        case _              =>
                    }
                    handle(a,b,exit,entry)
            }
            elts.last match {
                case DefaultStmt(body) => body >> next
                case CaseStmt(_,body)  => body >> next
                case _                 => elts.last >> next
            }
            switchBody
    }
    
    private def handleSwitchBranch(branch: SourceCodeNode, parent: CompoundStmt): Unit = branch match {
        case CaseStmt(_,c)  => parent >> branch; handleSwitchBranch(c,parent)
        case DefaultStmt(c) => parent >> branch; handleSwitchBranch(c,parent)
        case _              =>
    }
    
    private def handleSwitchCase(caseStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = caseStmt match {
        case CaseStmt(_,body)  => caseStmt >> handle(body,next,exit,entry); caseStmt
        case DefaultStmt(body) => caseStmt >> handle(body,next,exit,entry); caseStmt
    }
    
    private def handleExpr(node: SourceCodeNode, next: SourceCodeNode) = {
        node >> next
        node
    }
    
    private def handleJump(node: SourceCodeNode, exit: SourceCodeNode) = {
        node >> exit
        node
    }
    
}
