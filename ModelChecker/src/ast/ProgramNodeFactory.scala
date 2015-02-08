package ast

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => MSet}

import ast.model._
import ast.model.Expr
import cfg.GraphNode

class ProgramNodeFactory(rootNode: SourceCodeNode, labelNodes: Map[String,SourceCodeNode]) {    
    /**
     * Starts by converting every SourceCodeNode into a GraphNode while accumulating information about jump statements.
     * To finish, finalizes the graph by linking the jump statements origin(s) to their destination
     */
    
    private def cut(node: SourceCodeNode, set: MSet[SourceCodeNode]): Unit = {
        if (set contains node) return
        node match {
            case CompoundStmt(_) | IfStmt(_, _, _) | CaseStmt(_, _) |
                DefaultStmt(_) | ForStmt(_, _, _, _) | WhileStmt(_, _) |
                DoWhileStmt(_, _) | SwitchStmt(_, _) | BreakStmt() |
                ContinueStmt() | GotoStmt(_) | LabelStmt(_, _) | NullStmt() =>
                node.prev.foreach { y => y.next.remove(node); y.next ++= node.next }
                node.next.foreach { y => y.prev.remove(node); y.prev ++= node.prev }
            case _ => set += node
        }
        node.next.foreach(cut(_,set))
    }
    
    private def cut(node: SourceCodeNode): SourceCodeNode = {
        cut(node,MSet())
        node
    }
    
    lazy val result = cut(handle(rootNode,NullStmt(),None,None))

    def handle(node: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]): SourceCodeNode = {
        val res = node match {
        	case IfStmt(_,_,_)                     => handleIf(node,next,exit,entry)
	    	case ForStmt(_,_,_,_)                  => handleFor(node,next,exit,entry)
        	case WhileStmt(_,_)                    => handleWhile(node,next,exit,entry)
            case DoWhileStmt(_,_)                  => handleDoWhile(node,next,exit,entry)
            case CompoundStmt(_)                   => handleCompound(node,next,exit,entry)
            case ReturnStmt(_)                     => node
            case BreakStmt()                       => handleJump(node,exit)
            case ContinueStmt()                    => handleJump(node,entry)
            case GotoStmt(label)                   => handleJump(node,labelNodes(label))
            case SwitchStmt(_,_)                   => handleSwitch(node,next,(notNull(next) | entry),entry)
            case CaseStmt(_,_)                     => handleSwitchCase(node,next,exit,entry)
            case DefaultStmt(_)                    => handleSwitchCase(node,next,exit,entry)
            case LabelStmt(_,_)                    => handleLabel(node,next,exit,entry)
            case _                                 => handleExpr(node,next)
        }
        res
    }
    
    def notNull(node: SourceCodeNode) = if (node.isInstanceOf[NullStmt]) None else Some(node)
    
    private def findLeaves(node: SourceCodeNode): List[SourceCodeNode] = findLeaves(Some(node))
    private def findLeaves(node: Option[SourceCodeNode]): List[SourceCodeNode] = node match {
        case Some(x) => x match {
            case CompoundStmt(elts)          => if (elts.isEmpty) List(x) else findLeaves(elts.last)
            case IfStmt(cond,body,elseStmt)  => findLeaves(body) ++ findLeaves(elseStmt | cond)
            case CaseStmt(_,body)            => findLeaves(body)
            case DefaultStmt(body)           => findLeaves(body)
            case SwitchStmt(_,body)          => findLeaves(body)
            case ForStmt(_,cond,_,body)      => findLeaves(cond | body)
            case BreakStmt()                 => List()
            case ReturnStmt(_)               => List()
            case ContinueStmt()              => List()
            case _                           => List(x)
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
            } else {
                cmpdStmt >> next
            }
            cmpdStmt
    }
    
	private def handleIf(ifStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = ifStmt match {
       case IfStmt(condition,body,elseStmt) =>
            ifStmt >> condition
            condition >> handle(body, next, exit, entry)
            elseStmt match {
                case Some(x) => condition >> handle(x, next, exit, entry)
                case None    => condition >> (notNull(next) | entry)
            }
            ifStmt
    }
    
    private def handleFor(forStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = forStmt match {
        case ForStmt(init,cond,update,body) =>
            forStmt >> init >> cond >> handle(body,(update | cond | body).get,Some(next),(update | cond | body))
            body match {
                case CompoundStmt(elts) if elts.isEmpty => body >> update >> (cond | body)
                case _                                  => (cond | body).get << update
            }
            next << (cond | init | forStmt)
            forStmt
    }
	
	private def handleWhile(whileStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = whileStmt match {
	    case WhileStmt(condition,body) =>
            whileStmt >> condition
            condition >> handle(body,condition,Some(next),Some(condition))
            condition >> next
            body match {
                case CompoundStmt(elts) if elts.isEmpty => body >> condition
                case _                                  => 
            }
            whileStmt
	}
    
    private def handleDoWhile(doStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = doStmt match {
        case DoWhileStmt(condition,body) =>
            doStmt >> handle(body,condition,Some(next),Some(condition))
            condition >> next
            condition >> body
            body match {
                case CompoundStmt(elts) if elts.isEmpty => body >> condition
                case _                                  => 
            }
            doStmt
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
                        case _                              =>
                    }
                    handle(a,b,exit,entry)
            }
            elts.last match {
                case DefaultStmt(body) => body >> next
                case CaseStmt(_,body)  => findLeaves(body).foreach(_ >> next)
                case _                 => elts.last >> next
            }
            switchBody
    }
    
    private def handleSwitchBranch(branch: SourceCodeNode, parent: CompoundStmt): Unit = branch match {
            case CaseStmt(_, c) => parent >> branch; handleSwitchBranch(c, parent)
            case DefaultStmt(c) => parent >> branch; handleSwitchBranch(c, parent)
            case _              =>
    }
    
    private def handleSwitchCase(caseStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = caseStmt match {
        case CaseStmt(_,body)  => caseStmt >> handle(body,next,exit,entry); caseStmt
        case DefaultStmt(body) => caseStmt >> handle(body,next,exit,entry); caseStmt
    }
    
    private def handleLabel(labelStmt: SourceCodeNode, next: SourceCodeNode, exit: Option[SourceCodeNode], entry: Option[SourceCodeNode]) = labelStmt match {
        case LabelStmt(label,body) =>
            labelStmt >> handle(body,next,exit,entry)
            labelStmt
    }
    
    private def handleExpr(node: SourceCodeNode, next: SourceCodeNode) = {
        node >> next
        node
    }
    
    private def handleJump(node: SourceCodeNode, exit: SourceCodeNode) = {
        node >> exit
        node
    }
    
    private def handleJump(node: SourceCodeNode, exit: Option[SourceCodeNode]) = exit match {
        case Some(x) => node >> x; node
        case _       => node
    }
    
}
