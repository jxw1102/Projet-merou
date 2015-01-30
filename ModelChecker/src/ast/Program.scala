package ast

import cfg.GraphNode

class Program(var declarations: List[Decl]=List()) 
abstract class ProgramNode {
    def toGraph: GraphNode
}

object ProgramNode {
    def apply(node: ASTNode) = node match {
        case ConcreteASTNode(depth,ofType,id,pos,data) => 
            ofType match {
                case "CompoundStmt" => new CompoundStmt
                case "DeclStmt"     => //new DeclStmt
            }
    }
}