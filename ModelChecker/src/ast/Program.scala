package ast

import cfg.GraphNode

class Program(var declarations: List[Decl]=List()) 
abstract class ProgramNode {
    def toGraph: GraphNode
}