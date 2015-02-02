package ast

class StmtFactory {
    def handleASTNode(node: ASTNode): ProgramNode = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "CompoundStmt" => compoundStmt(node)
            case "IfStmt"       => ifStmt      (node)
            case "ForStmt"      => forStmt     (node)
            case "ExprStmt"     => exprStmt    (node)
        }
        case OtherASTNode(depth,data)        => ???
    }
    
	def forStmt(node: ASTNode) = ???
	def ifStmt(node: ASTNode) = node match {
	    case ConcreteASTNode(_,_,id,codeRange,_) => {
	    	val cond     = exprStmt(node.children(1))
	    	val body     = compoundStmt(node.children(2))
	    	val elseStmt = node.children(3) match {
	    		case ConcreteASTNode(_,_,_,_,_) => Some(compoundStmt(node.children(3)))
	    		case _                          => None
	    	}
	    	ProgramNode(IfStmt(cond,body,elseStmt),codeRange,id)
	    }
	    case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
	}
	
	def compoundStmt(node: ASTNode): CompoundStmt = CompoundStmt(node.children.map(handleASTNode).toList)
	def exprStmt(node: ASTNode) = ???
}