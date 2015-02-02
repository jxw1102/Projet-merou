package ast

class StmtFactory {
	def forStmt(node: ConcreteASTNode) = ???
	def ifStmt(node: ConcreteASTNode) = node match {
	    case ConcreteASTNode(_,_,id,codeRange,_) =>
	    	val cond     = exprStmt(node.children(1))
	    	val body     = compoundStmt(node.children(2))
	    	val elseStmt = node.children(3) match {
	    		case ConcreteASTNode(_,_,_,_,_) => Some(compoundStmt(node.children(3)))
	    		case NullASTNode(depth)         => None
	    	}
	    	ProgramNode(IfStmt(cond,body,elseStmt),codeRange,id)
	}
	
	def compoundStmt(node: ASTNode) = ???
	def exprStmt(node: ASTNode) = ???
}